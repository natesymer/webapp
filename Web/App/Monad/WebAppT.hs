{-# LANGUAGE OverloadedStrings, TupleSections #-}

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

{-
TODO

* Catch errors
* HTTP params

-}

{-# LANGUAGE TupleSections, Rank2Types #-}

module Web.App.Monad.WebAppT
(
  -- * Monad Transformers
  WebAppT(..),
  -- * Monadic Actions
  toApplication,
  middleware,
  route,
  get,
  post,
  put,
  patch,
  delete,
  options,
  anyRequest
) where

import Web.App.State
import Web.App.Monad.RouteT
import Web.App.Path
import Web.App.Stream

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM

import Network.Wai
import Network.Wai.HTTP2
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method

-- |Monad for defining routes & adding middleware.
newtype WebAppT s m a = WebAppT {
  runWebAppT :: [Route s m]
             -> [Middleware]
             -> (a,[Route s m],[Middleware])
}

instance (WebAppState s, Functor m) => Functor (WebAppT s m) where
  fmap f m = WebAppT $ \r mw -> let (a, r', mw') = runWebAppT m r mw
                                in (f a, r', mw')

instance (WebAppState s, Monad m) => Applicative (WebAppT s m) where
  pure a = WebAppT $ \r mw -> (a, r, mw)
  (<*>) = ap

instance (WebAppState s, Monad m) => Monad (WebAppT s m) where
  m >>= k = WebAppT $ \r mw ->
    let ~(a, r', mw')   = runWebAppT m r mw
        ~(b, r'', mw'') = runWebAppT (k a) r' mw'
    in (b, r'', mw'')
  fail msg = WebAppT $ \_ _ -> error msg

-- |Turn a WebAppT computation into a WAI 'Application'.
toApplication :: (WebAppState s, MonadIO m, MonadIO n)
              => (m RouteResult -> IO RouteResult) -- ^ fnc eval a monadic computation in @m@ in @IO@
              -> WebAppT s m () -- ^ a web app
              -> n (HTTP2Application, Application, TVar s) -- ^ resulting 'Application'
toApplication runToIO webapp = do
  st <- liftIO $ newTVarIO =<< initState
  let ~(_,rts,mws) = runWebAppT webapp [] []
      withMiddleware = foldl (flip ($)) (mkApp st rts) mws
      withMiddleware2 = mkApp2 st rts -- TODO add middleware
  return (withMiddleware2, withMiddleware, st)
  where
    -- TODO: error catching
    notFoundHeaders = [("Content-Type", "text/plain; charset=utf-8")]
    mkApp tvar routes req callback = case findRoute routes req of
      Nothing -> callback $ responseLBS status404 notFoundHeaders "Not found."
      Just (remainder,(_,pth,act)) -> do
        res <- runToIO $ evalRouteT act tvar pth nullPushFunc req
        case res of
          Nothing -> mkApp tvar remainder req callback
          Just (s,h,b) -> callback $ responseStream s h $ runStream b
        
    mkApp2 tvar routes req pushFunc = case findRoute routes req of
      Nothing -> respond status404 notFoundHeaders $ streamSimple $ runStream $ stream "Not found."
      Just (remainder,(_,pth,act)) -> respondIO $ do
        res <- runToIO $ evalRouteT act tvar pth (wrapPushFunc pushFunc routes) req
        case res of
          Nothing -> return $ mkApp2 tvar remainder req pushFunc
          Just (s,h,b) -> return $ respond s h $ streamSimple $ runStream b
    
-- |Use a middleware
middleware :: (WebAppState s, Monad m) => Middleware -> WebAppT s m ()
middleware m = WebAppT $ \r mw -> ((),r,mw ++ [m])

-- |Define a route
route :: (WebAppState s, Monad m) => Predicate -> Path -> RouteT s m () -> WebAppT s m ()
route p pth act = WebAppT $ \r mw -> ((),r ++ [(p,pth,act)],mw)

{-# INLINE matchMethod #-}
matchMethod :: Method -> Predicate
matchMethod meth = \r -> (requestMethod r) == meth

{- Monadic matchers -}

-- |Match a `GET` request.
get :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
get p act = route (matchMethod methodGet) p act

-- |Match a `POST` request.
post :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
post p act = route (matchMethod methodPost) p act

-- |Match a `PUT` request.
put :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
put p act = route (matchMethod methodPut) p act

-- |Match a `PATCH` request.
patch :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
patch p act = route (matchMethod methodPatch) p act

-- |Match a `DELETE` request.
delete :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
delete p act = route (matchMethod methodDelete) p act

-- |Match a `OPTIONS` request.
options :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
options p act = route (matchMethod methodOptions) p act

-- |Match any request.
anyRequest :: (WebAppState s, Monad m) => Path -> RouteT s m () -> WebAppT s m ()
anyRequest = route (const True)