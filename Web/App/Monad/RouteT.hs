{-# LANGUAGE OverloadedStrings, TupleSections #-}

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
  getState,
  setState,
  modifyState,
  request,
  addHeader,
  status,
  headers,
  params
)
where
  
import Web.App.State
  
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
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
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T

-- | Used to determine if a route can handle a request           
type Predicate = Request -> Bool

type RouteResult = (Status, ResponseHeaders, StreamingBody)

-- |Route monad transformer.
newtype RouteT s m a = RouteT {
  runRouteT :: (TVar s) -- ^ tvar containing state
            -> Maybe PushFunc -- ^ HTTP2 server push function; 'Nothing' when over HTTP1
            -> [(Predicate, RouteT s m ())] -- ^ all routes; used with the 'push' function
            -> Request -- ^ request being served
            -> m (a, Maybe Status, ResponseHeaders, StreamingBody)
}

instance (WebAppState s, Functor m) => Functor (RouteT s m) where
  fmap f m = RouteT $ \st pf rts req -> fmap (\(a,s,h,b) -> (f a,s,h,b)) $ runRouteT m st pf rts req
  
instance (WebAppState s, Monad m) => Applicative (RouteT s m) where
  pure a = RouteT $ \_ _ _ _ -> return (a,Nothing,[],nullBody)
  RouteT mf <*> RouteT mx = RouteT $ \st pf rts req -> do
    ~(f, s, h, bdy) <- mf st pf rts req
    ~(x, s', h', bdy') <- mx st pf rts req
    return (f x, seqStatus s s', h ++ h', appendBody bdy bdy')

instance (WebAppState s, Monad m) => Monad (RouteT s m) where
  fail msg = RouteT $ \_ _ _ -> fail msg
  m >>= k = RouteT $ \st pf rts req -> do
    ~(a, s, h, bdy) <- runRouteT m st pf rts req
    ~(b, s', h', bdy') <- runRouteT (k a) st pf rts req
    return (b, seqStatus s s', h ++ h', appendBody bdy bdy')
    
instance (WebAppState s) => MonadTrans (RouteT s) where
  lift m = RouteT $ \_ _ _ _ -> m >>= return . (,Nothing,[],nullBody)

instance (WebAppState s, MonadIO m) => MonadIO (RouteT s m) where
  liftIO = lift . liftIO
  
{- Status operations -}
  
seqStatus :: Maybe Status -> Maybe Status -> Maybe Status
seqStatus (Just a) Nothing = Just a
seqStatus Nothing (Just b) = Just b
seqStatus Nothing Nothing = Nothing
seqStatus (Just _) (Just b) = Just b
  
{- Body operations -}
  
nullBody :: StreamingBody
nullBody = \_ _ -> return ()

mkBody :: Builder -> StreamingBody
mkBody b = \w f -> (w b) >> f

appendBody :: StreamingBody -> StreamingBody -> StreamingBody
appendBody a b = \w f -> a w f >> b w f
  
{- Route Evaluation -}
  
toResponse :: (WebAppState s, Monad m, Monad n) => TVar s -- ^ initial state
                                                -> RouteT s m () -- ^ route action
                                                -> Request -- ^ incoming request
                                                -> (m RouteResult -> n RouteResult) -- ^ fnc to eval a monadic computation in @m@ in @n@
                                                -> n Response
toResponse st act req runToN = do
  ~(s,h,b) <- runToN (removeA <$> runRouteT act st Nothing [] req)
  return $ responseStream s h b
  where removeA (_,s',h',b') = (fromMaybe status200 s',h',b')
  
toResponder :: (WebAppState s, Monad m, Monad n) => TVar s -- ^ initial state
                                                 -> RouteT s m () -- ^ route action
                                                 -> Request -- ^ incoming request
                                                 -> PushFunc -- ^ request's push func
                                                 -> [(Predicate, RouteT s m ())] -- ^ all routes
                                                 -> (m RouteResult -> n RouteResult) -- ^ fnc to eval a monadic computation in @m@ in @n@
                                                 -> n Responder
toResponder st act req pushFunc rts runToN = do
  ~(s,h,b) <- runToN (removeA <$> runRouteT act st (Just pushFunc) rts req)
  return $ respond s h (streamSimple b)
  where removeA (_,s',h',b') = (fromMaybe status200 s',h',b')
  
{- Monadic actions -}

push :: (WebAppState s, MonadIO m) => Method -> ByteString -> RouteT s m Bool
push meth path = RouteT $ \st mpf rts req -> case mpf of
  Nothing -> return (False,Nothing,[],nullBody)
  Just pf -> case find (routePasses pushReq) rts of
    Nothing -> return (False,Nothing,[],nullBody)
    Just (_,act) -> do
      ~(s,h,b) <- (removeA <$> runRouteT act st (Just pf) rts pushReq)
      result <- liftIO $ pf (mkPromise meth path h) (mkResponder s h b)
      return (result,Nothing,[],nullBody)
      where removeA (_,s',h',b') = (fromMaybe status200 s',h',b')
            mkResponder s h b = (respond s h $ streamSimple b)
            mkPromise m p h = PushPromise m path authority scheme h
              where scheme = if isSecure req then "https" else "http"
                    authority = fromMaybe "" $ requestHeaderHost req
                    path = if m == methodOptions then "*" else p
  where routePasses r (p,_) = p r
        pushReq = defaultRequest { -- TODO better pathInfo parsing
          requestMethod = meth,
          rawPathInfo = path,
          rawQueryString = B.empty,
          pathInfo = map T.decodeUtf8 $ filter (not . B.null) $ B.split '/' path,
          queryString = [],
          isSecure = (isSecure req),
          httpVersion = (httpVersion req)}

writeBody :: (WebAppState s, MonadIO m) => Builder -> RouteT s m ()
writeBody builder = RouteT $ \_ _ _ _ -> return ((),Nothing,[],mkBody builder)
  
getState :: (WebAppState s, MonadIO m) => RouteT s m s
getState = RouteT $ \st _ _ _ -> do
  v <- liftIO $ readTVarIO st
  return (v,Nothing,[],nullBody)

setState :: (WebAppState s, MonadIO m) => s -> RouteT s m ()
setState newST = RouteT $ \st _ _ _ -> do
  liftIO $ atomically $ writeTVar st newST
  return ((),Nothing,[],nullBody)
  
modifyState :: (WebAppState s, MonadIO m) => (s -> s) -> RouteT s m ()
modifyState f = RouteT $ \st _ _ _ -> do
  liftIO $ atomically $ modifyTVar st f
  return ((),Nothing,[],nullBody)
  
request :: (WebAppState s, MonadIO m) => RouteT s m Request
request = RouteT $ \_ _ _ req -> return (req,Nothing,[],nullBody)

addHeader :: (WebAppState s, MonadIO m) => HeaderName -> ByteString -> RouteT s m ()
addHeader k v = RouteT $ \_ _ _ _ -> return ((),Nothing,[(k,v)],nullBody)

status :: (WebAppState s, MonadIO m) => Status -> RouteT s m ()
status s = RouteT $ \_ _ _ _ -> return ((),Just s,[],nullBody)

headers :: (WebAppState s, MonadIO m) => RouteT s m RequestHeaders
headers = fmap requestHeaders request
  
-- TODO: get from form as well
params :: (WebAppState s, MonadIO m) => RouteT s m Query
params = fmap queryString request

-- TODO: body