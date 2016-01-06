{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module Web.App.Monad.RouteT
(
  -- * RouteT monad transformer
  RouteT(..),
  evalRouteT,
  -- * Additional Types
  RouteResult,
  Predicate,
  Route,
  wrapPushFunc,
  findRoute,
  -- * Monadic actions
  push,
  writeBody,
  request,
  addHeader,
  status,
  headers,
  params,
  bodyReader,
  body,
  urlencodedBody,
  path
)
where
  
import Web.App.State
import Web.App.Path
import Web.App.Stream
  
import Control.Monad (ap)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Concurrent.STM
import Control.Applicative

import Network.Wai
import Network.Wai.HTTP2
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Network.HTTP.Types.Method

import Data.Maybe
import Data.List

import System.IO.Unsafe

import Blaze.ByteString.Builder
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Internal as BL (ByteString(Empty,Chunk))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as T
           
type Predicate = Request -> Bool -- ^ Used to determine if a route can handle a request
type Route s m = (Predicate, Path, RouteT s m ()) 
type WrappedPushFunc s m = (Request -> RouteT s m Bool)
type RouteResult = (Status, ResponseHeaders, Stream) -- kind of like a Ruby Rack response.

-- |RouteT monad transformer. All routes are evaluated
-- in this monad transformer.
newtype RouteT s m a = RouteT {
  runRouteT :: (TVar s) -- ^ tvar containing state
            -> Path -- ^ Path of route
            -> WrappedPushFunc s m -- ^ HTTP/2 server push function; Does nothing for HTTP/1.1 or HTTP/1
            -> Request -- ^ request being served
            -> m (a, Maybe Status, ResponseHeaders, Maybe Stream)
}

-- |Evaluate a 'RouteT' action into a 'RouteResult'.
evalRouteT :: (WebAppState s, MonadIO m)
           => RouteT s m () -- ^ route to evaluate
           -> (TVar s) -- ^ tvar containing state
           -> Path -- ^ Path of route
           -> WrappedPushFunc s m -- ^ HTTP2 server push function; 'Nothing' when over HTTP1
           -> Request -- ^ request being served
           -> m RouteResult
evalRouteT act st pth pf req = fmap removeA $ runRouteT act st pth pf req
  where removeA (_,s,h,b) = (fromMaybe status200 s,h,fromMaybe mempty b)

instance (WebAppState s, Functor m) => Functor (RouteT s m) where
  fmap f m = RouteT $ \st pth pf req -> fmap (\(a,s,h,b) -> (f a,s,h,b)) $ runRouteT m st pth pf req
  
instance (WebAppState s, Monad m) => Applicative (RouteT s m) where
  pure a = RouteT $ \_ _ _ _ -> return (a,Nothing,[],Nothing)
  (<*>) = ap

instance (WebAppState s, Monad m) => Monad (RouteT s m) where
  fail msg = RouteT $ \_ _ _ _ -> fail msg
  m >>= k = RouteT $ \st pth pf req -> do
    ~(x, s, h, b) <- runRouteT m st pth pf req
    ~(y, s', h', b') <- runRouteT (k x) st pth pf req
    return (y, s' <|> s, h ++ h', mappend b b')
      
instance (WebAppState s) => MonadTrans (RouteT s) where
  lift m = RouteT $ \_ _ _ _ -> m >>= return . (,Nothing,[],Nothing)

instance (WebAppState s, MonadIO m) => MonadIO (RouteT s m) where
  liftIO = lift . liftIO
  
instance (WebAppState s, MonadIO m) => MonadState s (RouteT s m) where
  get = RouteT $ \st _ _ _ -> (,Nothing,[],Nothing) <$> (liftIO $ atomically $ readTVar st)
  put v = RouteT $ \st _ _ _ -> (,Nothing,[],Nothing) <$> (liftIO $ atomically $ writeTVar st v)

instance (WebAppState s, MonadIO m) => MonadWriter Stream (RouteT s m) where
  tell s = RouteT $ \_ _ _ _ -> return ((),Nothing,[],Just s)
  listen act = RouteT $ \st pth pf req -> do
    (a,_,_,mw) <- runRouteT act st pth pf req
    return ((a,fromMaybe mempty mw),Nothing,[],Nothing)
  pass act = RouteT $ \st pth pf req -> do
    ((a,f),_,_,mw) <- runRouteT act st pth pf req
    return (a,Nothing,[],maybe mempty (Just . f) mw)
  
{- Route Evaluation -}
  
wrapPushFunc :: (WebAppState s, MonadIO m)
             => PushFunc -- ^ wai 'PushFunc' to wrap
             -> [Route s m] -- ^ routes that can be pushed
             -> WrappedPushFunc s m
wrapPushFunc pf rts = f
  where f req = RouteT $ \st _ _ _ -> do
          case findRoute rts req of
            Nothing -> return (False,Nothing,[],Nothing)
            Just (_,pth,act) -> do
              (s,h,b) <- evalRouteT act st pth f req
              let meth = requestMethod req
                  p = rawPathInfo req
                  scheme = if isSecure req then "https" else "http"
                  authority = fromMaybe "" $ requestHeaderHost req
                  promise = PushPromise meth p authority scheme h
                  responder = respond s h $ streamSimple $ runStream b
              (,Nothing,[],Nothing) <$> (liftIO $ pf promise responder)
              
findRoute :: (WebAppState s, Monad m) => [Route s m] -> Request -> Maybe (Route s m)
findRoute rts req = find (routePasses req) rts
  where routePasses r (pd,pth,_) = pd r && pathMatches pth (pathInfo req)

{- Monadic actions -}

-- |Push a resource using HTTP2 server push.
push :: (WebAppState s, MonadIO m)
     => Method -- ^ HTTP method used to access resource
     -> ByteString -- ^ path of resource
     -> RouteT s m Bool
push meth rpth = do
  (pf,req) <- RouteT $ \_ _ pf req -> return ((pf,req),Nothing,[],Nothing)
  res <- pf $ pushReq req
  return res
  where (pInfo,pQuery) = splitPath $ T.decodeUtf8 rpth
        pushReq req = req {
          requestMethod = meth,
          rawPathInfo = T.encodeUtf8 pInfo,
          rawQueryString = T.encodeUtf8 pQuery,
          pathInfo = mkPathInfo pInfo,
          queryString = mkQueryDict pQuery,
          requestBody = return B.empty,
          requestBodyLength = KnownLength 0,
          requestHeaderRange = Nothing,
          requestHeaders = [] -- should headers be left intact?
        }

-- |Write a builder to the response body.
writeBody :: (WebAppState s, Monad m)
          => Builder -- ^ builder to write
          -> RouteT s m ()
writeBody builder = RouteT $ \_ _ _ _ ->
  return ((),Nothing,[],Just $ stream builder)
  
-- |Get the 'Request' being served.
request :: (WebAppState s, Monad m) => RouteT s m Request
request = RouteT $ \_ _ _ req -> return (req,Nothing,[],Nothing)

-- |Add an HTTP header.
addHeader :: (WebAppState s, Monad m) => HeaderName -> ByteString -> RouteT s m ()
addHeader k v = RouteT $ \_ _ _ _ -> return ((),Nothing,[(k,v)],Nothing)

-- |Set the HTTP status.
status :: (WebAppState s, Monad m) => Status -> RouteT s m ()
status s = RouteT $ \_ _ _ _ -> return ((),Just s,[],Nothing)

-- |Get the request's headers.
headers :: (WebAppState s, Monad m) => RouteT s m RequestHeaders
headers = fmap requestHeaders request
  
-- TODO: get from form as well
params :: (WebAppState s, Monad m) => RouteT s m Query
params = fmap queryString request

-- |Get an action to read a chunk from the HTTP response body.
bodyReader :: (WebAppState s, Monad m) => RouteT s m (IO ByteString)
bodyReader = fmap requestBody request

-- |Get the body as a lazy byteString
body :: (WebAppState s, MonadIO m) => RouteT s m BL.ByteString
body = bodyReader >>= liftIO . lazyRead
  where
    lazyRead f = unsafeInterleaveIO $ do
      c <- f
      if B.null c
        then return BL.Empty
        else BL.Chunk c <$> (lazyRead f)

urlencodedBody :: (WebAppState s, MonadIO m) => RouteT s m Query
urlencodedBody = (mkQueryDict . T.decodeUtf8 . BL.toStrict) <$> body

path :: (WebAppState s, Monad m) => RouteT s m Path
path = RouteT $ \_ pth _ _ -> return (pth,Nothing,[],Nothing)