{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module Web.App.Monad.RouteT
(
  -- * RouteT monad transformer
  RouteT(..),
  -- * Intermediate Types
  RouteResult,
  Predicate,
  -- * WAI
  toResponse,
  toResponder,
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
  urlencodedBodyReader,
  urlencodedBody,
  path
)
where
  
import Web.App.State
import Web.App.Path
import Web.App.Streaming
  
import Control.Monad (ap)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Concurrent.STM

import Network.Wai
import Network.Wai.HTTP2
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Network.HTTP.Types.Method

import Data.Maybe
import Data.List

import Blaze.ByteString.Builder
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text.Encoding as T
           
type Predicate = Request -> Bool -- ^ Used to determine if a route can handle a request
type Route s m a = (Predicate, Path, RouteT s m a) 
type WrappedPushFunc = (Request -> IO Bool)
type RouteResult = (Status, ResponseHeaders, Stream) -- kind of like a Ruby Rack response.

-- |RouteT monad transformer. All routes are evaluated
-- in this monad transformer.
newtype RouteT s m a = RouteT {
  runRouteT :: (TVar s) -- ^ tvar containing state
            -> Path -- ^ Path of route
            -> Maybe WrappedPushFunc -- ^ HTTP2 server push function; 'Nothing' when over HTTP1
            -> Request -- ^ request being served
            -> m (a, Maybe Status, ResponseHeaders, Maybe Stream)
}

instance (WebAppState s, Functor m) => Functor (RouteT s m) where
  fmap f m = RouteT $ \st pth pf req -> fmap (\(a,s,h,b) -> (f a,s,h,b)) $ runRouteT m st pth pf req
  
instance (WebAppState s, Monad m) => Applicative (RouteT s m) where
  pure a = RouteT $ \_ _ _ _ -> return (a,Nothing,[],Nothing)
  (<*>) = ap

instance (WebAppState s, Monad m) => Monad (RouteT s m) where
  fail msg = RouteT $ \_ _ _ _ -> fail msg
  m >>= k = RouteT $ \st pth pf req -> do
    ~(a, s, h, bdy) <- runRouteT m st pth pf req
    ~(b, s', h', bdy') <- runRouteT (k a) st pth pf req
    return (b, seqMaybes (flip const) s s', h ++ h', seqMaybes mappend bdy bdy')
      
instance (WebAppState s) => MonadTrans (RouteT s) where
  lift m = RouteT $ \_ _ _ _ -> m >>= return . (,Nothing,[],Nothing)

instance (WebAppState s, MonadIO m) => MonadIO (RouteT s m) where
  liftIO = lift . liftIO
  
instance (WebAppState s, MonadIO m) => MonadState s (RouteT s m) where
  get = RouteT $ \st _ _ _ -> (,Nothing,[],Nothing) <$> (liftIO $ atomically $ readTVar st)
  put v = RouteT $ \st _ _ _ -> (,Nothing,[],Nothing) <$> (liftIO $ atomically $ writeTVar st v)

instance (WebAppState s, MonadIO m) => MonadState Stream (RouteT s m) where
  tell s = RouteT $ \_ _ _ _ -> return ((),Nothing,[],Just s)
  listen act = RouteT $ \st pth pf req -> do
    (a,_,_,mw) <- runRouteT act st pth pf req
    return ((a,fromMaybe mempty mw),Nothing,[],Nothing)
  pass act = RouteT $ \st pth pf req -> do
    ((a,f),_,_,mw) <- runRouteT act st pth pf req
    return (a,Nothing,[],maybe mempty f mw)

seqMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
seqMaybes _ Nothing  Nothing  = Nothing
seqMaybes _ (Just a) Nothing  = Just a
seqMaybes _ Nothing  (Just b) = Just b
seqMaybes f (Just a) (Just b) = Just $ f a b
  
{- Route Evaluation -}

evalRouteT :: (WebAppState s, Monad m)
           => RouteT s m ()
           -> (TVar s) -- ^ tvar containing state
           -> Path -- ^ Path of route
           -> Maybe WrappedPushFunc-- ^ HTTP2 server push function; 'Nothing' when over HTTP1
           -> Request -- ^ request being served
           -> m RouteResult
evalRouteT act st pth pf req = fmap removeA $ runRouteT act st pth pf req
  where removeA (_,s,h,b) = (fromMaybe status200 s,h,fromMaybe mempty b)

toResponse :: (WebAppState s, Monad m, Monad n)
           => TVar s -- ^ initial state
           -> RouteT s m () -- ^ route action
           -> Request -- ^ incoming request
           -> Path -- ^ matched path
           -> (m RouteResult -> n RouteResult) -- ^ fnc to eval a monadic computation in @m@ in @n@
           -> n Response
toResponse st act req pth runToN = do
  (s,h,b) <- runToN $ evalRouteT act st pth Nothing req
  return $ responseStream s h $ runStream b

toResponder :: (WebAppState s, MonadIO m, Monad n)
            => TVar s -- ^ initial state
            -> RouteT s m () -- ^ route action
            -> [Route s m ()] -- ^ routes used to wrap a 'PushFunc'
            -> Request -- ^ incoming request
            -> PushFunc -- ^ request's push func (see 'wrapPushFunc')
            -> Path -- ^ matched path
            -> (m RouteResult -> n RouteResult) -- ^ fnc to eval a monadic computation in @m@ in @n@
            -> n Responder
toResponder st act rts req pushFunc pth runToN = do
  (s,h,b) <- runToN $ evalRouteT act st pth (Just $ wrapPushFunc st pushFunc rts) req
  return $ respond s h (streamSimple $ runStream b)
  
-- |Wrap a WAI 'PushFunc' in routes & state
wrapPushFunc :: (WebAppState s, MonadIO m)
             => TVar s -- ^ state
             -> PushFunc -- ^ 'PushFunc' to wrap
             -> [Route s m ()] -- ^ routes in which to wrap 'PushFunc'
             -> WrappedPushFunc
wrapPushFunc st pf rts = f st
  where f st req = case find (routePasses req) rts of
          Nothing -> return False
          Just (_,pth,act) -> do
            (s,h,b) <- evalRouteT act st pth (Just $ f st) req
            let meth = requestMethod req
                path = rawPathInfo req
                scheme = if isSecure req then "https" else "http"
                authority = fromJust $ requestHeaderHost req
                promise = PushPromise meth path authority scheme h
                responder = respond s h $ streamSimple $ runStream b
            pf promise responder
          where routePasses r (p,_,_) = p r
  
{- Monadic actions -}

-- |Push a resource using HTTP2 server push.
push :: (WebAppState s, Monad m)
     => Method -- ^ HTTP method used to access resource
     -> ByteString -- ^ path of resource
     -> RouteT s m Bool
push meth rpth = RouteT actf
  where actf _ _ Nothing   _   = return (False,Nothing,[],Nothing)
        actf _ _ (Just pf) req = pf pushReq
          where (pInfo,pQuery) = splitPath $ T.decodeUtf8 rpth
                pushReq = defaultRequest {
                  requestMethod = meth,
                  rawPathInfo = T.encodeUtf8 pInfo,
                  rawQueryString = T.encodeUtf8 pQuery,
                  pathInfo = mkPathInfo pInfo,
                  queryString = mkQueryDict pQuery,
                  isSecure = isSecure req,
                  httpVersion = httpVersion req}

-- |Write a builder to the response body.
writeBody :: (WebAppState s, Monad m)
          => Builder -- ^ 
          -> RouteT s m ()
writeBody builder = RouteT $ \_ _ _ _ ->
  return ((),Nothing,[],Just $ stream builder)
  
-- |Get the 'Request' being served.
request :: (WebAppState s, Monad m) => RouteT s m Request
request = RouteT $ \_ _ _ req -> return (req,Nothing,[],Nothing)

-- |Add an HTTP header
addHeader :: (WebAppState s, Monad m) => HeaderName -> ByteString -> RouteT s m ()
addHeader k v = RouteT $ \_ _ _ _ -> return ((),Nothing,[(k,v)],Nothing)

status :: (WebAppState s, Monad m) => Status -> RouteT s m ()
status s = RouteT $ \_ _ _ _ -> return ((),Just s,[],Nothing)

headers :: (WebAppState s, Monad m) => RouteT s m RequestHeaders
headers = fmap requestHeaders request
  
-- TODO: get from form as well
params :: (WebAppState s, Monad m) => RouteT s m Query
params = fmap queryString request

bodyReader :: (WebAppState s, Monad m) => RouteT s m (IO ByteString)
bodyReader = fmap requestBody request

body :: (WebAppState s, MonadIO m) => RouteT s m ByteString
body = bodyReader >>= liftIO

urlencodedBodyReader :: (WebAppState s, Monad m) => RouteT s m (IO Query)
urlencodedBodyReader = fmap (mkQueryDict . T.decodeUtf8) <$> bodyReader

urlencodedBody :: (WebAppState s, MonadIO m) => RouteT s m Query
urlencodedBody = urlencodedBodyReader >>= liftIO

path :: (WebAppState s, Monad m) => RouteT s m Path
path = RouteT $ \_ pth _ _ -> return (pth,Nothing,[],Nothing)