{-|
Module      : Web.App.Monad.WebAppT
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Defines a monad transformer used for defining routes
and using middleware.
-}

{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module Web.App.Monad.RouteT
(
  -- * RouteT monad transformer
  RouteT,
  evalRouteT,
  -- * Additional Types
  RouteInterrupt(..),
  RouteResult,
  Predicate,
  Route,
  -- * Routing convenience functions
  wrapPushFunc,
  nullPushFunc,
  findRoute,
  -- * Monadic actions
  halt,
  halt',
  next,
  push,
  writeBody,
  writeJSON,
  request,
  addHeader,
  status,
  headers,
  header,
  redirect,
  params,
  param,
  maybeParam,
  bodyReader,
  body,
  urlencodedBody,
  path
)
where
  
{-

TODO

* rewrite param & maybeParam to use something other than 'Read'.
  
-}
  
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
import Data.Monoid
import Data.Aeson
import Data.CaseInsensitive (mk)

import System.IO.Unsafe

import Blaze.ByteString.Builder
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Internal as BL (ByteString(Empty,Chunk))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as T

{-
TODO

  * Allow 'InterruptNext' to carry state into
    the evaluation of the next route.

-}
           
type Predicate = Request -> Bool -- ^ Used to determine if a route can handle a request
type Route s m = (Predicate, Path, RouteT s m ()) 
type WrappedPushFunc s m = (Request -> RouteT s m Bool)
type RouteResult = Maybe (Status, ResponseHeaders, Stream) -- kind of like a Ruby Rack response.

data RouteInterrupt = InterruptNext -- ^ halt current route evaluation and start evaluating next route
                    | InterruptHalt (Maybe Status) ResponseHeaders (Maybe Stream) -- ^ halt & provide a response

-- |Monad transformer in which routes are evaluated. It's essentially
-- an ExceptT crossed with an RWST with the path, body, and push func
-- as the "Reader" state, the response as the "Writer" state, and no
-- "State" state.
newtype RouteT s m a = RouteT {
  runRouteT :: TVar s -- ^ tvar containing state
            -> Path -- ^ path of route
            -> TVar BL.ByteString -- ^ request body
            -> WrappedPushFunc s m -- ^ HTTP/2 server push function; Does nothing for HTTP/1.1 or HTTP/1
            -> Request -- ^ request being served
            -> m (Either RouteInterrupt (a, Maybe Status, ResponseHeaders, Maybe Stream))
}

-- |Evaluate a 'RouteT' action into a 'RouteResult'.
evalRouteT :: (WebAppState s, MonadIO m)
           => RouteT s m () -- ^ route to evaluate
           -> TVar s -- ^ tvar containing state
           -> Path -- ^ path of route
           -> WrappedPushFunc s m -- ^ wrapped HTTP/2 server push function
           -> Request -- ^ request being served
           -> m (RouteResult)
evalRouteT act st pth pf req = do
  bdy <- liftIO $ newTVarIO BL.Empty
  v <- runRouteT act st pth bdy pf req
  case v of
    Left InterruptNext -> return Nothing
    Left (InterruptHalt s h b) -> return $ Just (fromMaybe status200 s,h,fromMaybe mempty b)
    Right ~(_,s,h,b) -> return $ Just (fromMaybe status200 s,h,fromMaybe mempty b)
            
instance (WebAppState s, Functor m) => Functor (RouteT s m) where
  fmap f m = RouteT $ \st pth bdy pf req -> fmap apply $ runRouteT m st pth bdy pf req
    where
      apply (Right (a,s,h,b)) = Right (f a,s,h,b)
      apply (Left e) = Left e
  
instance (WebAppState s, Monad m) => Applicative (RouteT s m) where
  pure a = RouteT $ \_ _ _ _ _ -> return $ Right (a,Nothing,[],Nothing)
  (<*>) = ap

instance (WebAppState s, Monad m) => Monad (RouteT s m) where
  fail msg = RouteT $ \_ _ _ _ _ -> fail msg
  m >>= k = RouteT $ \st pth bdy pf req -> do
    v <- runRouteT m st pth bdy pf req
    case v of
      Left e -> return $ Left e
      Right ~(x, s, h, b) -> do
        v' <- runRouteT (k x) st pth bdy pf req
        case v' of
          Left InterruptNext -> return $ Left InterruptNext
          Left (InterruptHalt s' h' b') -> return $ Left combined
            where combined = InterruptHalt (s' <|> s) (h' <> h) (b <> b')
          Right ~(y, s', h', b') -> return $ Right $ combined
            where combined = (y, s' <|> s, h' <> h, b <> b')

instance (WebAppState s) => MonadTrans (RouteT s) where
  lift m = RouteT $ \_ _ _ _ _ -> m >>= return . Right . (,Nothing,[],Nothing)

instance (WebAppState s, MonadIO m) => MonadIO (RouteT s m) where
  liftIO = lift . liftIO
  
instance (WebAppState s, MonadIO m) => MonadState s (RouteT s m) where
  get = RouteT $ \st _ _ _ _ -> Right . (,Nothing,[],Nothing) <$> (liftIO $ atomically $ readTVar st)
  put v = RouteT $ \st _ _ _ _ -> Right . (,Nothing,[],Nothing) <$> (liftIO $ atomically $ writeTVar st v)

instance (WebAppState s, Monad m) => MonadWriter Stream (RouteT s m) where
  tell s = RouteT $ \_ _ _ _ _ -> return $ Right ((),Nothing,[],Just s)
  listen act = RouteT $ \st pth bdy pf req -> do
    v <- runRouteT act st pth bdy pf req
    case v of
      Left e -> return $ Left e
      Right (a,_,_,mw) -> return $ Right ((a,fromMaybe mempty mw),Nothing,[],Nothing)
  pass act = RouteT $ \st pth bdy pf req -> do
    v <- runRouteT act st pth bdy pf req
    case v of
      Left e -> return $ Left e
      Right ((a,f),_,_,mw) -> return $ Right (a,Nothing,[],maybe mempty (Just . f) mw)

{- Route Evaluation -}
  
-- |A 'WrappedPushFunc' that does nothing. For use in situations
-- that don't support HTTP/2 Server Push
nullPushFunc :: (WebAppState s, Monad m) => WrappedPushFunc s m
nullPushFunc = const $ return False

-- |Wrap a 'PushFunc' in 'RouteT' and a list of 'Route's.
wrapPushFunc :: (WebAppState s, MonadIO m)
             => PushFunc -- ^ wai 'PushFunc' to wrap
             -> [Route s m] -- ^ routes that can be pushed
             -> WrappedPushFunc s m
wrapPushFunc pf rts = f
  where f req = RouteT $ \st _ _ _ _ -> do
          case findRoute rts req of
            Nothing -> return $ Right (False,Nothing,[],Nothing)
            Just (_,(_,pth,act)) -> do
              e <- evalRouteT act st pth f req
              case e of
                Nothing -> return $ Right (False,Nothing,[],Nothing) -- TODO: recurse
                Just (s,h,b) -> do
                  let meth = requestMethod req
                      p = rawPathInfo req
                      scheme = if isSecure req then "https" else "http"
                      authority = fromMaybe "" $ requestHeaderHost req
                      promise = PushPromise meth p authority scheme h
                      responder = respond s h $ streamSimple $ runStream b
                  Right . (,Nothing,[],Nothing) <$> (liftIO $ pf promise responder)
                    
findRoute :: (WebAppState s, Monad m) => [Route s m] -> Request -> Maybe ([Route s m],Route s m)
findRoute [] _ = Nothing
findRoute (x@(pd,pth,_):xs) req
  | pd req && pathMatches pth (pathInfo req) = Just (xs,x)
  | otherwise = findRoute xs req

{- Monadic actions -}

-- |Halt route evaluation and provide the given 'Status',
-- 'ResponseHeaders', and 'Stream'.
halt :: (WebAppState s, Monad m) => Status -> ResponseHeaders -> Stream -> RouteT s m ()
halt s h b = RouteT $ \_ _ _ _ _ -> return $ Left $ InterruptHalt (Just s) h (Just b)
  
-- |Halt route evaluation and provide the accumulated 'Status',
-- 'ResponseHeaders', and 'Stream'.
halt' :: (WebAppState s, Monad m) => RouteT s m ()
halt' = RouteT $ \_ _ _ _ _ -> return $ Left $ InterruptHalt Nothing [] Nothing

-- |Halt route evaluation and move onto the next
-- route that passes.
next :: (WebAppState s, Monad m) => RouteT s m ()
next = RouteT $ \_ _ _ _ _ -> return $ Left InterruptNext

-- |Push a resource using HTTP2 server push.
push :: (WebAppState s, MonadIO m)
     => Method -- ^ HTTP method used to access resource
     -> ByteString -- ^ path of resource
     -> RouteT s m Bool
push meth rpth = do
  let (pInfo,pQuery) = splitPath $ T.decodeUtf8 rpth
  (pf,req) <- RouteT $ \_ _ _ pf req -> return $ Right ((pf,req),Nothing,[],Nothing)
  pf $ req {
    requestMethod = meth,
    rawPathInfo = T.encodeUtf8 pInfo,
    rawQueryString = T.encodeUtf8 pQuery,
    pathInfo = mkPathInfo pInfo,
    queryString = mkQueryDict pQuery,
    requestBody = return B.empty,
    requestBodyLength = KnownLength 0,
    requestHeaderRange = Nothing,
    requestHeaders = []} -- should headers be left intact?

-- |Write a builder to the response body.
writeBody :: (WebAppState s, Monad m)
          => Builder -- ^ builder to write
          -> RouteT s m ()
writeBody builder = RouteT $ \_ _ _ _ _ ->
  return $ Right ((),Nothing,[],Just $ stream builder)
  
-- |Write a JSON object to the response body.
writeJSON :: (WebAppState s, Monad m, ToJSON j)
          => j -- ^ json object to write
          -> RouteT s m ()
writeJSON = writeBody . fromLazyByteString . encode

-- |Get the 'Request' being served.
request :: (WebAppState s, Monad m) => RouteT s m Request
request = RouteT $ \_ _ _ _ req -> return $ Right (req,Nothing,[],Nothing)

-- |Add an HTTP header.
addHeader :: (WebAppState s, Monad m) => HeaderName -> ByteString -> RouteT s m ()
addHeader k v = RouteT $ \_ _ _ _ _ -> return $ Right ((),Nothing,[(k,v)],Nothing)

-- |Set the HTTP status.
status :: (WebAppState s, Monad m) => Status -> RouteT s m ()
status s = RouteT $ \_ _ _ _ _ -> return $ Right ((),Just s,[],Nothing)

-- |Redirect to the given path using a @Location@ header and
-- an HTTP status of 302. Route evaluation continues.
redirect :: (WebAppState s, MonadIO m) => ByteString -> RouteT s m ()
redirect url = halt status302 [("Location",url)] mempty
  
-- |Get the 'Request''s headers.
headers :: (WebAppState s, Monad m) => RouteT s m RequestHeaders
headers = requestHeaders <$> request

-- |Get a specific header.
header :: (WebAppState s, Monad m) => ByteString -> RouteT s m (Maybe ByteString)
header k = lookup (mk k) <$> headers

-- |Get the 'Request''s parameters (in order captures, HTTP body, URI query).
params :: (WebAppState s, MonadIO m) => RouteT s m Query
params = fmap (mconcat . catMaybes) $ sequence [cap,bdy,q]
  where
    cap = RouteT $ \_ pth _ _ req -> return $ Right (Just $ map toQuery $ pathCaptures pth (pathInfo req),Nothing,[],Nothing)
      where toQuery (a,b) = (T.encodeUtf8 a, Just $ T.encodeUtf8 b)
    bdy = do
      h <- requestHeaders <$> request
      case lookup (mk "Content-Type") h of
        Just "application/x-www-form-urlencoded" -> fmap (Just . parseQuery . BL.toStrict) body
        Just "multipart/form-data" -> return Nothing -- TODO: implement me
        Just _ -> return Nothing
        Nothing -> return Nothing
    q = Just . queryString <$> request

-- |Get a specific header. Will call 'next' if the parameter isn't present.
param :: (WebAppState s, MonadIO m, Read a) => ByteString -> RouteT s m a
param k = params >>= f . lookup k
  where f (Just (Just v)) = return $ read $ B.unpack v
        f _ = next >> (return $ read "")
        
-- |Get a specific header. Will not interfere with route evaluation.
maybeParam :: (WebAppState s, MonadIO m, Read a) => ByteString -> RouteT s m (Maybe a)
maybeParam k = maybe Nothing (fmap (read . B.unpack)) . lookup k <$> params

-- |Get an action that reads a chunk from the HTTP body. Can be used
-- before 'body'. A chunk is not read until it's needed (non-strictness).
bodyReader :: (WebAppState s, MonadIO m) => RouteT s m (IO ByteString)
bodyReader = RouteT $ \_ _ bdy _ req -> return $ Right (act bdy (requestBody req),Nothing,[],Nothing)
  where
    act tv f = do
      c <- f
      atomically $ readTVar tv >>= writeTVar tv . BL.Chunk c
      return c

-- |Get the body as a lazy byteString. Can be used after
-- reading individual chunks from the HTTP body.
body :: (WebAppState s, MonadIO m) => RouteT s m BL.ByteString
body = RouteT $ \_ _ bdy _ req -> liftIO $ do
  remainder <- lazyRead $ requestBody req
  atomically $ do
    alreadyRead <- readTVar bdy
    let whole = alreadyRead <> remainder
    writeTVar bdy whole
    return $ Right (whole,Nothing,[],Nothing)
  where
    lazyRead f = unsafeInterleaveIO $ do
      c <- f
      if B.null c
        then return BL.Empty
        else BL.Chunk c <$> (lazyRead f)

urlencodedBody :: (WebAppState s, MonadIO m) => RouteT s m Query
urlencodedBody = (mkQueryDict . T.decodeUtf8 . BL.toStrict) <$> body

path :: (WebAppState s, Monad m) => RouteT s m Path
path = RouteT $ \_ pth _ _ _ -> return $ Right (pth,Nothing,[],Nothing)