{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

{-|
Module      : Web.App.Monad.WebAppT
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Defines a monad transformer used for defining routes
and using middleware
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
import Network.HTTP.Types.Status (status404,status200)
    
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
toApplication :: (WebAppState s, MonadIO m) => TVar s -- ^ initial state
                                          -> WebAppT s m () -- ^ calculation
                                          -> m Application -- ^ resulting 'Application'
toApplication tvar act = do
  ~(_,routes,middlewares) <- runWebAppT act [] []
  f (mkApp tvar routes) middlewares
  where
    f :: (MonadIO m) => Application -> [Middleware] -> m Application
    f app [] = return app
    f app (x:xs) = f (x app) xs
    mkApp :: (WebAppState s, MonadIO m) => TVar s -> [(Predicate, RouteT s m ())] -> Application
    mkApp st routes = \req respond -> case find (routePasses req) routes of
      -- Just (_, ra) -> evalRouteT st ra req >>= respond -- TODO: FIXME m1 vs IO
      Just (_, ra) -> do
        resp <- evalRouteT st ra req -- TODO: FIXME m1 vs IO (91:31 - ra)
        respond resp
      Nothing -> respond $ responseLBS status404 [] "Not found."
      where routePasses r (p,_) = p r
      
-- |Use a middleware
middleware :: (WebAppState s, Monad m) => Middleware -> WebAppT s m ()
middleware m = WebAppT $ \r mw -> return ((),r,mw ++ [m])

-- |Define a route
route :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
route p act = WebAppT $ \r mw -> return ((),r ++ [(p,act)],mw)