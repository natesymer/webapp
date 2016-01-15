{-|
Module      : Web.App.Monad.WebAppT
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Defines a monoid used for defining routes
and using middleware.
-}

{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleInstances #-}

module Web.App.WebApp
(
  WebApp(..),
  toApplication,
  -- * Web App Operations
  middleware,
  route,
  get,
  post,
  put,
  patch,
  delete,
  options,
  anyRequest,
  matchAll
) where

import Web.App.State
import Web.App.RouteT
import Web.App.Path
import Web.App.Stream

import Control.Monad.IO.Class
import Control.Concurrent.STM

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method

import Data.Monoid

-- |Monoid for defining routes & adding middleware.
newtype WebApp s m = WebApp { runWebApp :: ([Route s m],[Middleware]) }

instance (WebAppState s, Monad m) => Monoid (WebApp s m) where
  mempty = WebApp ([],[])
  mappend (WebApp (r,mw)) (WebApp (r',mw')) = WebApp (r <> r',mw <> mw')

-- |Turn a 'WebAppT' computation into a WAI 'Application'.
toApplication :: (WebAppState s, MonadIO m, MonadIO n)
              => (m RouteResult -> IO RouteResult) -- ^ fnc eval a monadic computation in @m@ in @IO@
              -> WebApp s m -- ^ a web app
              -> n (Application, -- ^ WAI application
                    IO ()) -- ^ teardown action; call when shutting down app server
toApplication runToIO webapp = do
  st <- liftIO $ newTVarIO =<< initState
  let ~(rts,mws) = runWebApp webapp
      app = foldl (flip ($)) (mkApp st rts) mws
      teardown = readTVarIO st >>= destroyState
  return (app, teardown)
  where
    plainText = [("Content-Type", "text/plain; charset=utf-8")]
    mkApp tvar routes req callback = case findRoute routes req of
      Nothing -> callback $ responseLBS status404 plainText "Not found."
      Just (remainder,(_,pth,act)) -> do
        res <- runToIO $ evalRouteT act tvar pth req
        case res of
          Nothing -> mkApp tvar remainder req callback
          Just (s,h,b) -> callback $ responseStream s h $ runStream b

-- |Use a middleware
middleware :: (WebAppState s, Monad m) => Middleware -> WebApp s m
middleware m = WebApp ([],[m])

-- |Define a route
route :: (WebAppState s, Monad m) => Predicate -> Path -> RouteT s m () -> WebApp s m
route p pth act = WebApp ([(p,pth,act)],[])

{-# INLINE matchMethod #-}
matchMethod :: Method -> Predicate
matchMethod meth = \r -> (requestMethod r) == meth

{- Monadic matchers -}

-- |Match a `GET` request.
get :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebApp s m
get p act = route (matchMethod methodGet) p act

-- |Match a `POST` request.
post :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebApp s m
post p act = route (matchMethod methodPost) p act

-- |Match a `PUT` request.
put :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebApp s m
put p act = route (matchMethod methodPut) p act

-- |Match a `PATCH` request.
patch :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebApp s m
patch p act = route (matchMethod methodPatch) p act

-- |Match a `DELETE` request.
delete :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebApp s m
delete p act = route (matchMethod methodDelete) p act

-- |Match a `OPTIONS` request.
options :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebApp s m
options p act = route (matchMethod methodOptions) p act

-- |Match any request given a path.
anyRequest :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebApp s m
anyRequest = route (const True)

-- |Match all requests and paths.
matchAll :: (WebAppState s, Monad m) => RouteT s m () -> WebApp s m
matchAll = route (const True) (regex ".*")