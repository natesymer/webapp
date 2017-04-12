{-|
Module      : Web.App.RouteT
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Monad transformer used for defining web app routes.
-}

{-# LANGUAGE OverloadedStrings, TupleSections, BangPatterns #-}

module Web.App.RouteT
(
  toApplication,
  -- * Types
  RouteT,
  Route(..),
  -- * Routes
  get,
  post,
  put,
  patch,
  delete,
  options,
  method,
  matchAll,
  -- * Monadic actions
  abort,
  finish,
  next,
  writeBody,
  request,
  isTLS,
  addHeader,
  status,
  headers,
  header,
  redirect,
  params,
  param,
  maybeParam,
  body,
  getState,
  putState
)
where

import Web.App.State
import Web.App.Path
import Web.App.Stream
import Web.App.Parameter

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Concurrent.STM

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Network.HTTP.Types.Method

import Data.Bool
import Data.Monoid
import Data.Functor

import Data.IORef
import Data.Vault.Lazy (Vault, Key)
import qualified Data.Vault.Lazy as V
import Data.CaseInsensitive (mk)

import System.IO.Unsafe

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Internal as BL (ByteString(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as T

data Route s m = Route {
  _routePredicate :: Request -> Bool,
  _routePath :: Path,
  _routeAction :: RouteT s m ()
}

data EvalState = Normal -- ^ continue executing as normal
               | Next -- ^ skip to the next route
               | Finish -- ^ stop evaluation and return what's been accumulated.
               | Abort -- ^ stop evaluation altogether, setting new HTTP response
  deriving (Eq)

data ResponseChange = SetStatus Status
                    | AddHeaders [Header]
                    | AddBytes Stream

-- | INTERNAL: apply ResponseChanges to an empty response and return the result
flattenResponse :: [ResponseChange] -> Response
flattenResponse = f (status200, [], mempty)
  where f (s, hdrs, sm) [] = responseStream s hdrs $ runStream sm
        f (_, hdrs, sm) ((SetStatus s):rs) = f (s, hdrs, sm) rs
        f (s, hdrs, sm) ((AddHeaders hdrs'):rs) = f (s, hdrs ++ hdrs', sm) rs
        f (s, hdrs, sm) ((AddBytes bs):rs) = f (s, hdrs, sm <> bs) rs

-- |Monad transformer in which routes are evaluated. Evaluation may be stopped
-- via 'abort', 'finish', 'next', or 'redirect'.
newtype RouteT s m a = RouteT {
  runRouteT :: TVar s -- ^ tvar containing state
            -> Bool -- ^ whether or not a certificate was installed
            -> Path -- ^ path of route
            -> Request -- ^ request being served
            -> m (a, (EvalState, [ResponseChange]))
}

instance (WebAppState s, Functor m) => Functor (RouteT s m) where
  fmap f m = RouteT $ \st sec pth req -> fmap apply $ runRouteT m st sec pth req
    where apply (a, c) = (f a, c)
  
instance (WebAppState s, Monad m) => Applicative (RouteT s m) where
  pure a = RouteT $ \_ _ _ _ -> return (a, (Normal, mempty))
  (<*>) = ap

instance (WebAppState s, Monad m) => Monad (RouteT s m) where
  fail msg = RouteT $ \_ _ _ _ -> fail msg
  m >>= k = RouteT actionf
    where
      actionf st sec pth req = runRouteT m st sec pth req >>= handleFirst
        where
          handleFirst (x, (Normal, cng1)) = runRouteT (k x) st sec pth req >>= handleSecond
            where
              handleSecond (x', (sts, cng2)) = return (x', (sts, cng3))
                where cng3 = bool (cng1 <> cng2) cng2 $ sts == Abort
          handleFirst (_, c) = return (undefined, c)

instance (WebAppState s) => MonadTrans (RouteT s) where
  lift m = RouteT $ \_ _ _ _ -> m >>= return . (,(Normal, mempty))

instance (WebAppState s, MonadIO m) => MonadIO (RouteT s m) where
  liftIO = lift . liftIO

{- Monadic actions -}

-- |INTERNAL A helper to avoid having the RouteT constructor appear everywhere (DRY)
{-# INLINE context #-}
context :: (WebAppState s, Monad m) => EvalState -> [ResponseChange] -> RouteT s m ()
context st hc = RouteT $ \_ _ _ _ -> return ((), (st, hc))

-- |INTERNAL Get the state TVar.
stateTVar :: (WebAppState s, Monad m) => RouteT s m (TVar s)
stateTVar = RouteT $ \st _ _ _ -> return (st, (Normal, mempty))

-- |INTERNAL Get the route's path. This can be different across different evaluations of the same route.
path :: (WebAppState s, Monad m) => RouteT s m Path
path = RouteT $ \_ _ pth _ -> return (pth, (Normal, mempty))

-- |Get the 'Request' being served.
request :: (WebAppState s, Monad m) => RouteT s m Request
request = RouteT $ \_ _ _ req -> return (req, (Normal, mempty))

-- |Whether or not the server is using certificates
isTLS :: (WebAppState s, Monad m) => RouteT s m Bool
isTLS = RouteT $ \_ sec _ _ -> return (sec, (Normal, mempty))

-- | Get the web app state.
getState :: (WebAppState s, MonadIO m) => RouteT s m s
getState = liftIO . readTVarIO =<< stateTVar

-- | Set the web app state.
putState :: (WebAppState s, MonadIO m) => s -> RouteT s m ()
putState v = liftIO . atomically . flip writeTVar v =<< stateTVar

-- |Add an HTTP header.
addHeader :: (WebAppState s, Monad m) => HeaderName -> ByteString -> RouteT s m ()
addHeader k v = context Normal [AddHeaders [(k, v)]]

-- |Set the HTTP status.
status :: (WebAppState s, Monad m) => Status -> RouteT s m ()
status s = context Normal [SetStatus s]

-- |Write a 'Stream' to the response body.
writeBody :: (WebAppState s, Monad m, ToStream w) => w -> RouteT s m ()
writeBody w = context Normal [AddBytes (stream' w)]

-- |Halt route evaluation and provide a 'Status', '[Header]', and 'Stream'.
abort :: (WebAppState s, Monad m) => Status -> [Header] -> Stream -> RouteT s m a
abort s h b = context Abort [SetStatus s, AddBytes b, AddHeaders h] >> undefined

-- |Halt route evaluation and return the accumulated HTTP response.
finish :: (WebAppState s, Monad m) => RouteT s m a
finish = context Finish mempty >> undefined

-- |Halt route evaluation and move onto the next matched route.
next :: (WebAppState s, Monad m) => RouteT s m a
next = context Next [] >> undefined

-- |Redirect to the given path using a @Location@ header and
-- an HTTP status of 302. Route evaluation halts.
{-# INLINE redirect #-}
redirect :: (WebAppState s, MonadIO m) => ByteString -> RouteT s m ()
redirect url = status status302 >> addHeader "Location" url >> finish
 
-- |Get the 'Request''s headers.
headers :: (WebAppState s, Monad m) => RouteT s m [Header]
headers = requestHeaders <$> request

-- |Get a specific header.
header :: (WebAppState s, Monad m) => ByteString -> RouteT s m (Maybe ByteString)
header k = lookup (mk k) <$> headers

-- |Read the 'Request''s parameters (in order captures, HTTP body, URI query).
params :: (WebAppState s, MonadIO m) => RouteT s m Query
params = readAll -- maybe (readAll >>= insertMutVault cachedParamsKey) return =<< lookupMutVault cachedParamsKey
  where
    readAll = mconcat <$> sequence [cap, bdy, queryString <$> request]
    cap = map toQueryItem <$> (pathCaptures <$> path <*> (pathInfo <$> request))
    bdy = request >>= maybe (return []) bodyParamsFor . lookup (mk "Content-Type") . requestHeaders
    bodyParamsFor "application/x-www-form-urlencoded" = parseQuery . BL.toStrict <$> body
    bodyParamsFor "multipart/form-data" = return [] -- TODO: implement me
    bodyParamsFor _ = return []
    toQueryItem (a, b) = (T.encodeUtf8 a, Just $ T.encodeUtf8 b)

-- |Get a specific header. Will call 'next' if the parameter isn't present.
param :: (WebAppState s, MonadIO m, Parameter a) => ByteString -> RouteT s m a
param k = maybeParam k >>= maybe next return

-- |Get a specific header. Will not interfere with route evaluation.
maybeParam :: (WebAppState s, MonadIO m, Parameter a) => ByteString -> RouteT s m (Maybe a)
maybeParam k = f . lookup k <$> params where f x = join x >>= maybeRead

-- |Read the request body as a lazy 'ByteString'.
body :: (WebAppState s, MonadIO m) => RouteT s m BL.ByteString
body = maybe (request >>= liftIO . lazyRead . requestBody >>= insertMutVault cachedBodyKey) return =<< lookupMutVault cachedBodyKey
  where lazyRead rd = unsafeInterleaveIO $ rd >>= f
          where f c = bool (BL.Chunk c <$> lazyRead rd) (return BL.Empty) $ B.null c

{- ROUTE DEFINITION -}

-- |Match all requests and paths.
matchAll :: (WebAppState s, Monad m) => RouteT s m () -> Route s m
matchAll = Route (const True) (regex ".*")

{-# INLINE get #-}
{-# INLINE post #-}
{-# INLINE put #-}
{-# INLINE patch #-}
{-# INLINE delete #-}
{-# INLINE options #-}
get,post,put,patch,delete,options :: (WebAppState s, Monad m) => Path -> RouteT s m () -> Route s m
get        = method methodGet
post       = method methodPost
put        = method methodPut
patch      = method methodPatch
delete     = method methodDelete
options    = method methodOptions

-- |Match a route based on the request's HTTP method.
method :: (WebAppState s, Monad m) => Method -> Path -> RouteT s m () -> Route s m
method m = Route $ (==) m . requestMethod

{- Route Evaluation -}

-- |Makes an 'Application' from routes and middleware.
toApplication :: (WebAppState s, Monad m)
              => (m (Maybe Response) -> IO (Maybe Response)) -- ^ run your monadic type to IO
              -> Bool -- ^ whether or not the server is using TLS
              -> [Route s m] -- ^ routes
              -> [Middleware] -- ^ middlewares
              -> IO (Application, IO ()) -- ^ (application, teardown action)
toApplication runToIO sec routes mws = do
  st <- newTVarIO =<< initState
  return (app' st, readTVarIO st >>= destroyState)
  where
    plainText = [("Content-Type", "text/plain; charset=utf-8")]
    app' st = foldl (flip ($)) (app st) mws
    app st req callback = go =<< findSuccessful routeMatches runRoute routes
      where
        runRoute (Route _ pth act) = runToIO $ toResponse <$> runRouteT act st sec pth (addMutVault req)
        routeMatches (Route pd pth _) = pd req && pathMatches pth (pathInfo req)
        toResponse (_, (Next, _)) = Nothing
        toResponse (_, (_, cng)) = Just $ flattenResponse cng
        go Nothing = callback $ responseLBS status404 [] mempty
        go (Just (Left e)) = callback $ responseLBS status500 plainText $ BL.pack $ show (e :: SomeException)
        go (Just (Right jawn)) = callback jawn

-- | INTERNAL Find an a that passes a predicate and either errors out or returns something. Returns the error or the something.
findSuccessful :: (Exception e) => (a -> Bool) -> (a -> IO (Maybe b)) -> [a] -> IO (Maybe (Either e b))
findSuccessful _ _ [] = return Nothing
findSuccessful p f (x:xs) = if p x then (try $ f x) >>= go else findSuccessful p f xs
    where
      go (Right Nothing) = findSuccessful p f xs
      go (Right (Just v)) = return $ Just $ Right v
      go (Left e) = return $ Just $ Left e
    
    
{- Vault (internal) -}

-- Used to implement framework functionality    
          
cachedBodyKey :: Key BL.ByteString
cachedBodyKey = unsafePerformIO V.newKey
{-# NOINLINE cachedBodyKey #-}

cachedParamsKey :: Key Query
cachedParamsKey = unsafePerformIO V.newKey
{-# NOINLINE cachedParamsKey #-}
              
-- Used to implement mutable Vault.
              
mutableVaultKey :: Key (IORef Vault)
mutableVaultKey = unsafePerformIO V.newKey
{-# NOINLINE mutableVaultKey #-}

-- | Adds a "mutable" 'Vault' (@'IORef' 'Vault'@) to a 'Request'.
addMutVault :: Request -> Request
addMutVault r = r { vault = V.insert mutableVaultKey ioRef (vault r) }
  where ioRef = unsafePerformIO $ newIORef V.empty
        {-# NOINLINE ioRef #-}

-- | Exposes the "mutable" 'Vault' for modification/access. DRY.
withMutVaultM :: (WebAppState s, MonadIO m) => (Vault -> RouteT s m (Maybe a, Vault)) -> RouteT s m (Maybe a)
withMutVaultM xform = request >>= maybe (return Nothing) f . V.lookup mutableVaultKey . vault
  where f ior = (liftIO $ readIORef ior) >>= xform >>= uncurry (g ior)
        g ior ret vlt' = (liftIO $ writeIORef ior vlt') $> ret

lookupMutVault :: (WebAppState s, MonadIO m) => Key a -> RouteT s m (Maybe a)
lookupMutVault k = withMutVaultM $ \vlt -> return (V.lookup k vlt, vlt)

insertMutVault :: (WebAppState s, MonadIO m) => Key a -> a -> RouteT s m a
insertMutVault k v = (withMutVaultM $ return . (Nothing,) . V.insert k v) $> v
