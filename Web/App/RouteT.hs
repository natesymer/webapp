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

{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module Web.App.RouteT
(
  -- * Re-Exported Types
  Middleware,
  -- * RouteT monad transformer
  RouteT,
  evalRouteT,
  -- * Routes
  RouteResult,
  Predicate,
  Route,
  toApplication,
  -- * Routes
  routeMatches,
  route,
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
import Control.Applicative

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Network.HTTP.Types.Method

import Data.Bool
import Data.Maybe
import Data.Monoid

import Data.Aeson
import Data.CaseInsensitive (mk)

import System.IO.Unsafe

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Internal as BL (ByteString(Empty,Chunk))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as T

type Predicate = Request -> Bool -- ^ Used to determine if a route can handle a request
type Route s m = (Predicate, Path, RouteT s m ()) 
type RouteResult = Maybe (Status, ResponseHeaders, Stream) -- kind of like a Ruby Rack response.

data EvalState = Current -- continue executing as normal
               | Next -- skip to the next route
               | Halt -- stop evaluation altogether
  deriving (Eq)

data HTTPContext = HTTPContext {
  httpContextStatus :: Maybe Status,
  httpContextHeaders :: ResponseHeaders,
  httpContextBody :: Maybe Stream
}

instance Monoid HTTPContext where
  mempty = HTTPContext Nothing [] Nothing
  mappend (HTTPContext s1 h1 b1) (HTTPContext s2 h2 b2) = HTTPContext (s2 <|> s1) (h2 <|> h1) (b1 <> b2)
    
toRouteResult :: HTTPContext -> RouteResult
toRouteResult (HTTPContext s h b) = Just (fromMaybe status200 s, h, maybe mempty flush b)

data Context = Context {
  contextState :: EvalState,
  contextCachedBody :: Maybe BL.ByteString,
  contextHTTPContext :: HTTPContext
}

contextResult :: Context -> RouteResult
contextResult (Context Next _ _) = Nothing
contextResult (Context _ _ hc) = toRouteResult hc

freshContext :: Context
freshContext = contextWith mempty

contextWith :: HTTPContext -> Context
contextWith = Context Current Nothing

-- |Monad transformer in which routes are evaluated. It's essentially
-- an ExceptT crossed with an RWST with the path, body, and push func
-- as the "Reader" state, the response as the "Writer" state, and no
-- "State" state.
newtype RouteT s m a = RouteT {
  runRouteT :: TVar s -- ^ tvar containing state
            -> Path -- ^ path of route
            -> Maybe BL.ByteString -- ^ cached request body
            -> Request -- ^ request being served
            -> m (a, Context)
}

-- |Evaluate a 'RouteT' action into a 'RouteResult'.
evalRouteT :: (WebAppState s, Monad m)
           => RouteT s m () -- ^ route to evaluate
           -> TVar s -- ^ tvar containing state
           -> Path -- ^ path of route
           -> Request -- ^ request being served
           -> m RouteResult
evalRouteT act st pth req = contextResult . snd <$> runRouteT act st pth Nothing req

instance (WebAppState s, Functor m) => Functor (RouteT s m) where
  fmap f m = RouteT $ \st pth bdy req -> apply <$> runRouteT m st pth bdy req
    where apply (a, c) = (f a, c)
  
instance (WebAppState s, Monad m) => Applicative (RouteT s m) where
  pure a = RouteT $ \_ _ _ _ -> return (a, freshContext)
  (<*>) = ap

instance (WebAppState s, Monad m) => Monad (RouteT s m) where
  fail msg = RouteT $ \_ _ _ _ -> fail msg
  m >>= k = RouteT $ \st pth bdy req -> do
    v <- runRouteT m st pth bdy req
    case v of
      (x, Context Current bdy' c1) -> do
        (x', Context sts bdy'' c2) <- runRouteT (k x) st pth bdy' req
        if sts == Halt
          then return (x', Context sts bdy'' c2)
          else return (x', Context sts bdy'' (c1 <> c2))
      (_, c) -> return (undefined, c)

instance (WebAppState s) => MonadTrans (RouteT s) where
  lift m = RouteT $ \_ _ _ _ -> m >>= return . (,freshContext)

instance (WebAppState s, MonadIO m) => MonadIO (RouteT s m) where
  liftIO = lift . liftIO

{- Monadic actions -}

-- INTERNAL

path :: (WebAppState s, Monad m) => RouteT s m Path
path = RouteT $ \_ pth _ _ -> return (pth, freshContext)

cachedBody :: (WebAppState s, Monad m) => RouteT s m (Maybe BL.ByteString)
cachedBody = RouteT $ \_ _ b _ -> return (b, freshContext)

stateTVar :: (WebAppState s, Monad m) => RouteT s m (TVar s)
stateTVar = RouteT $ \st _ _ _ -> return (st, freshContext)

context :: (WebAppState s, Monad m) => EvalState -> HTTPContext -> RouteT s m ()
context st hc = RouteT $ \_ _ _ _ -> return ((), Context st Nothing hc)

-- EXTERNAL

-- |Get the 'Request' being served.
request :: (WebAppState s, Monad m) => RouteT s m Request
request = RouteT $ \_ _ _ req -> return (req, freshContext)

-- | Get the web app state.
getState :: (WebAppState s, MonadIO m) => RouteT s m s
getState = liftIO . readTVarIO =<< stateTVar

-- | Set the web app state.
putState :: (WebAppState s, MonadIO m) => s -> RouteT s m ()
putState v = liftIO . atomically . flip writeTVar v =<< stateTVar

-- |Add an HTTP header.
addHeader :: (WebAppState s, Monad m) => HeaderName -> ByteString -> RouteT s m ()
addHeader k v = context Current $ HTTPContext Nothing [(k,v)] Nothing

-- |Set the HTTP status.
status :: (WebAppState s, Monad m) => Status -> RouteT s m ()
status s = context Current $ HTTPContext (Just s) [] Nothing

-- |Write a 'Stream' to the response body.
{-# SPECIALIZE writeBody :: (WebAppState s, Monad m) => Stream -> RouteT s m () #-}
{-# SPECIALIZE writeBody :: (WebAppState s, Monad m) => String -> RouteT s m () #-}
{-# SPECIALIZE writeBody :: (WebAppState s, Monad m) => ByteString -> RouteT s m () #-}
writeBody :: (WebAppState s, Monad m, ToStream w) => w -> RouteT s m ()
writeBody w = context Current $ HTTPContext Nothing [] $ Just $ stream' w

-- |Write a JSON object to the response body.
writeJSON :: (WebAppState s, Monad m, ToJSON j) => j -> RouteT s m ()
writeJSON = writeBody . encode

-- |Halt route evaluation and provide a 'Status', 'ResponseHeaders', and 'Stream'.
halt :: (WebAppState s, Monad m) => Status -> ResponseHeaders -> Stream -> RouteT s m a
halt s h b = (context Halt $ HTTPContext (Just s) h (Just b)) >> undefined

-- |Halt route evaluation and move onto the next matched route.
next :: (WebAppState s, Monad m) => RouteT s m a
next = context Next mempty >> undefined

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

-- TODO FIXME: param reading has to be optimized. All params are parsed every single time
-- a single param is requested.

-- |Get the 'Request''s parameters (in order captures, HTTP body, URI query).
params :: (WebAppState s, MonadIO m) => RouteT s m Query
params = mconcat <$> sequence [cap, bdy, q]
  where
    q = queryString <$> request
    cap = map toQueryItem <$> (pathCaptures <$> path <*> (pathInfo <$> request))
    bdy = request >>= maybe (return []) bodyParamsFor . lookup (mk "Content-Type") . requestHeaders
    bodyParamsFor "application/x-www-form-urlencoded" = parseQuery . BL.toStrict <$> body
    bodyParamsFor "multipart/form-data" = return [] -- TODO: implement me
    bodyParamsFor "application/json" = return [] -- TODO: implement me
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
body = maybe (request >>= liftIO . lazyRead . requestBody) return =<< cachedBody
  where lazyRead rd = unsafeInterleaveIO $ rd >>= f
          where f c = bool (BL.Chunk c <$> lazyRead rd) (return BL.Empty) $ B.null c

{- ROUTE DEFINITION -}

-- |Define a route
route :: (WebAppState s, Monad m) => Predicate -> Path -> RouteT s m () -> Route s m
route = (,,)

-- |Match all requests and paths.
matchAll :: (WebAppState s, Monad m) => RouteT s m () -> Route s m
matchAll = route (const True) (regex ".*")

get,post,put,patch,delete,options,anyRequest :: (WebAppState s, Monad m) => Path -> RouteT s m () -> Route s m
get        = route $ matchMethod methodGet
post       = route $ matchMethod methodPost
put        = route $ matchMethod methodPut
patch      = route $ matchMethod methodPatch
delete     = route $ matchMethod methodDelete
options    = route $ matchMethod methodOptions
anyRequest = route $ const True

-- INTERNAL
{-# INLINE matchMethod #-}
matchMethod :: Method -> Predicate
matchMethod meth r = meth == requestMethod r

{- Route Evaluation -}

-- |Determine if a 'Route' matches a 'Request'.
routeMatches :: (WebAppState s, Monad m) => Request -> Route s m -> Bool
routeMatches req (pd, pth, _) = pd req && pathMatches pth (pathInfo req)

toApplication :: (WebAppState s, Monad m, MonadIO n)
              => (m RouteResult -> IO RouteResult) -- ^ run your custom monad to IO
              -> [Route s m] -- ^ routes
              -> [Middleware] -- ^ middlewares
              -> n (Application, -- ^ WAI application
                    IO ()) -- ^ teardown action; call when shutting down app server
toApplication runToIO routes mws = do
  st <- liftIO $ newTVarIO =<< initState
  return (foldl (flip ($)) (app st) mws, readTVarIO st >>= destroyState)
  where
    plainText = [("Content-Type", "text/plain; charset=utf-8")]
    app st req callback = run $ filter (routeMatches req) routes
      where
        run [] = callback $ responseLBS status404 plainText "Not found."
        run ((_, pth, act):rs) = maybe (run rs) return =<< runRoute pth act
        runRoute pth act = go =<< (try $ runToIO $ evalRouteT act st pth req)
          where go (Left (e :: SomeException)) = Just <$> (callback $ responseLBS status500 plainText $ BL.pack $ show e)
                go (Right (Just (s, h, b))) = Just <$> (callback $ responseStream s h $ runStream b)
                go _ = return Nothing
