{-|
Module      : Web.App.Monad.WebAppT
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Monad transformer used for defining web app routes.
-}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Web.App.RouteT
(
  toApplication,
  -- * Types
  RouteT,
  Route(..),
  -- * Routes
  routeMatches,
  get,
  post,
  put,
  patch,
  delete,
  options,
  anyRequest,
  matchAll,
  -- * Monadic actions
  halt,
  next,
  writeBody,
  request,
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
               | Halt -- ^ stop evaluation altogether
  deriving (Eq)

data ResponseChange = SetStatus Status
                    | AddHeaders [Header]
                    | AddBytes Stream
                    -- | Next
                    -- | Halt Status [Header] Stream
                    -- | Multiple [ResponseChange]

flattenResponse :: [ResponseChange] -> Response
flattenResponse = g . f (status200, [], mempty)
  where f acc [] = acc
        f (_, hdrs, sm) ((SetStatus s):rs) = f (s, hdrs, sm) rs
        f (s, hdrs, sm) ((AddHeaders hdrs'):rs) = f (s, hdrs ++ hdrs', sm) rs
        f (s, hdrs, sm) ((AddBytes bs):rs) = f (s, hdrs, sm <> bs) rs
        -- f _             (Next:_) = f (status200, [], mempty) []
        -- f _             ((Halt s h b):_) = f (s, h, b) []
        g (s,hdrs,sm) = responseStream s hdrs $ runStream sm

-- |Monad transformer in which routes are evaluated. Evaluation may be stopped
-- via 'halt', 'next', or 'redirect'.
newtype RouteT s m a = RouteT {
  runRouteT :: TVar s -- ^ tvar containing state
            -> Path -- ^ path of route
            -> Request -- ^ request being served
            -> m (a, (EvalState, [ResponseChange]))
}

instance (WebAppState s, Functor m) => Functor (RouteT s m) where
  fmap f m = RouteT $ \st pth req -> fmap apply $ runRouteT m st pth req
    where apply (a, c) = (f a, c)
  
instance (WebAppState s, Monad m) => Applicative (RouteT s m) where
  pure a = RouteT $ \_ _ _ -> return (a, (Normal, mempty))
  (<*>) = ap

instance (WebAppState s, Monad m) => Monad (RouteT s m) where
  fail msg = RouteT $ \_ _ _ -> fail msg
  m >>= k = RouteT actionf
    where
      actionf st pth req = runRouteT m st pth req >>= handleFirst
        where
          handleFirst (x, (Normal, cng1)) = runRouteT (k x) st pth req >>= handleSecond
            where
              handleSecond (x', (sts, cng2)) = return (x', (sts, cng3))
                where cng3 = bool (cng1 <> cng2) cng2 $ sts == Halt
          handleFirst (_, c) = return (undefined, c)

instance (WebAppState s) => MonadTrans (RouteT s) where
  lift m = RouteT $ \_ _ _ -> m >>= return . (,(Normal, mempty))

instance (WebAppState s, MonadIO m) => MonadIO (RouteT s m) where
  liftIO = lift . liftIO

{- Monadic actions -}

-- INTERNAL

context :: (WebAppState s, Monad m) => EvalState -> [ResponseChange] -> RouteT s m ()
context st hc = RouteT $ \_ _ _ -> return ((), (st, hc))

stateTVar :: (WebAppState s, Monad m) => RouteT s m (TVar s)
stateTVar = RouteT $ \st _ _ -> return (st, (Normal, mempty))

path :: (WebAppState s, Monad m) => RouteT s m Path
path = RouteT $ \_ pth _ -> return (pth, (Normal, mempty))

-- EXTERNAL

-- |Get the 'Request' being served.
request :: (WebAppState s, Monad m) => RouteT s m Request
request = RouteT $ \_ _ req -> return (req, (Normal, mempty))

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

-- |Halt route evaluation and provide a 'Status', 'ResponseHeaders', and 'Stream'.
halt :: (WebAppState s, Monad m) => Status -> ResponseHeaders -> Stream -> RouteT s m a
halt s h b = context Halt [SetStatus s, AddBytes b, AddHeaders h] >> undefined

-- |Halt route evaluation and move onto the next matched route.
next :: (WebAppState s, Monad m) => RouteT s m a
next = context Next [] >> undefined

-- |Redirect to the given path using a @Location@ header and
-- an HTTP status of 302. Route evaluation halts.
redirect :: (WebAppState s, MonadIO m) => ByteString -> RouteT s m ()
redirect url = halt status302 [("Location",url)] mempty
  
-- |Get the 'Request''s headers.
headers :: (WebAppState s, Monad m) => RouteT s m RequestHeaders
headers = requestHeaders <$> request

-- |Get a specific header.
header :: (WebAppState s, Monad m) => ByteString -> RouteT s m (Maybe ByteString)
header k = lookup (mk k) <$> headers

-- |Read the 'Request''s parameters (in order captures, HTTP body, URI query).
params :: (WebAppState s, MonadIO m) => RouteT s m Query
params = maybe (readAll >>= insertMutVault cachedParamsKey) return =<< lookupMutVault cachedParamsKey
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

get,post,put,patch,delete,options,anyRequest :: (WebAppState s, Monad m) => Path -> RouteT s m () -> Route s m
get        = Route $ matchMethod methodGet
post       = Route $ matchMethod methodPost
put        = Route $ matchMethod methodPut
patch      = Route $ matchMethod methodPatch
delete     = Route $ matchMethod methodDelete
options    = Route $ matchMethod methodOptions
anyRequest = Route $ const True

{-# INLINE matchMethod #-}
matchMethod :: Method -> (Request -> Bool)
matchMethod meth r = meth == requestMethod r

{- Route Evaluation -}

-- |Determine if a 'Route' matches a 'Request'.
routeMatches :: (WebAppState s, Monad m) => Request -> Route s m -> Bool
routeMatches req (Route pd pth _) = pd req && pathMatches pth (pathInfo req)

-- |Makes an 'Application' from routes and middleware.
toApplication :: (WebAppState s, Monad m)
              => (m (Maybe Response) -> IO (Maybe Response)) -- ^ run your monadic type to IO
              -> [Route s m] -- ^ routes
              -> [Middleware] -- ^ middlewares
              -> (Application, IO ()) -- ^ (application, teardown action)
toApplication runToIO routes mws = (app', readTVarIO st >>= destroyState)
  where
    st = unsafePerformIO (newTVarIO =<< initState)
    {-# NOINLINE st #-}
    plainText = [("Content-Type", "text/plain; charset=utf-8")]
    app' = foldl (flip ($)) app mws
    app req callback = run $ filter (routeMatches req) routes
      where
        run [] = callback $ responseLBS status404 [] mempty
        run ((Route _ pth act):rs) = go =<< (try $ runToIO $ fmap f $ runRouteT act st pth (addMutVault req))
          where go (Left e) = callback $ responseLBS status500 plainText $ BL.pack $ show (e :: SomeException)
                go (Right jawn) = maybe (run rs) callback jawn
                f (_, (Next, _)) = Nothing
                f (_, (_, cng)) = Just $ flattenResponse cng

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

-- | Adds a "mutable vault" (IORef-wrapped vault) to a Request
addMutVault :: Request -> Request
addMutVault r = r { vault = V.insert mutableVaultKey ioRef (vault r) }
  where ioRef = unsafePerformIO $ newIORef V.empty
        {-# NOINLINE ioRef #-}

withMutVaultM :: (WebAppState s, MonadIO m) => (Vault -> RouteT s m (Maybe a, Vault)) -> RouteT s m (Maybe a)
withMutVaultM xform = request >>= maybe (return Nothing) f . V.lookup mutableVaultKey . vault
  where f ior = (liftIO $ readIORef ior) >>= xform >>= uncurry (g ior)
        g ior ret vlt' = (liftIO $ writeIORef ior vlt') $> ret

lookupMutVault :: (WebAppState s, MonadIO m) => Key a -> RouteT s m (Maybe a)
lookupMutVault k = withMutVaultM $ \vlt -> return (V.lookup k vlt, vlt)

insertMutVault :: (WebAppState s, MonadIO m) => Key a -> a -> RouteT s m a
insertMutVault k v = (withMutVaultM $ return . (Nothing,) . V.insert k v) $> v
