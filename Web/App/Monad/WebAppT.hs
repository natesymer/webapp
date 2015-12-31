{-# LANGUAGE OverloadedStrings #-}

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

* Route patterns (IE match a static path or a regex)
* Generalize from IO
* Errors!
* HTTP2 & HTTP2 server push
* HTTP params

-}

{-# LANGUAGE TupleSections, Rank2Types #-}

module Web.App.Monad.WebAppT
(
  -- * Monad Transformers
  WebAppT(..),
  -- * Typeclasses
  WebAppState(..),
  -- * Monadic Actions
  toApplication,
  middleware,
  route
) where

import Web.App.State
import Web.App.Monad.RouteT

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Concurrent.STM

import Data.List

import Network.Wai
import Network.HTTP.Types.Status (status404)

-- | Used to determine if a route can handle a request           
type Predicate = Request -> Bool
               
-- |Monad for defining routes & adding middleware.
newtype WebAppT s m a = WebAppT {
  runWebAppT :: [(Predicate, RouteT s m ())]
             -> [Middleware]
             -> m (a,[(Predicate, RouteT s m ())],[Middleware])
}

instance (WebAppState s, Functor m) => Functor (WebAppT s m) where
  fmap f m = WebAppT $ \r mw -> fmap (\(a, r', mw') -> (f a, r', mw')) $ runWebAppT m r mw

instance (WebAppState s, Monad m) => Applicative (WebAppT s m) where
  pure a = WebAppT $ \r mw -> pure (a, r, mw)
  (<*>) = ap
  -- WebAppT mf <*> WebAppT mx = WebAppT $ \r mw do
  --   ~(f, r', mw') <- mf r mw
  --   ~(x, r'', mw'') <- mx r' mw'
  --   return (f x, r'', mw'')

instance (WebAppState s, Monad m) => Monad (WebAppT s m) where
  m >>= k = WebAppT $ \r mw -> do
    ~(a, r', mw') <- runWebAppT m r mw
    ~(b, r'', mw'') <- runWebAppT (k a) r' mw'
    return (b, r'', mw'')
  fail msg = WebAppT $ \_ _ -> fail msg
  
instance (WebAppState s) => MonadTrans (WebAppT s) where
  lift m = WebAppT $ \r mw -> m >>= return . (,r,mw)

instance (WebAppState s, MonadIO m) => MonadIO (WebAppT s m) where
  liftIO = lift . liftIO

-- |Turn a WebAppT computation into a WAI 'Application'.
toApplication :: (WebAppState s, Monad m, Monad n) => TVar s -- ^ initial state
                                                  -> (m Response -> IO Response) -- ^ action to eval a monadic computation in @m@ in @IO@
                                                  -> (m Application -> n Application) -- ^ action to eval a monadic computation in @m@ in @IO@
                                                  -> WebAppT s m () -- ^ a web app
                                                  -> n Application -- ^ resulting 'Application'
toApplication tvar respToIO appToN act = appToN $ do
  ~(_,routes,middlewares) <- runWebAppT act [] []
  let statefulRoutes = map (\(p,a) -> (p,evalRouteT tvar a)) routes
      application = mkApp statefulRoutes
  return $ f application middlewares
  where
    f :: Application -> [Middleware] -> Application
    f app [] = app
    f app (x:xs) = f (x app) xs
    mkApp routes req respond = app
      where app = maybe notFound found $ find (routePasses req) routes
            routePasses r (p,_) = p r
            routeResponse r (_,a) = a r
            notFound = respond $ responseLBS status404 [] "Not found."
            found rt = (respToIO $ routeResponse req rt) >>= respond 
      
-- |Use a middleware
middleware :: (WebAppState s, Monad m) => Middleware -> WebAppT s m ()
middleware m = WebAppT $ \r mw -> return ((),r,mw ++ [m])

-- |Define a route
route :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
route p act = WebAppT $ \r mw -> return ((),r ++ [(p,act)],mw)