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

* Errors!
* HTTP2 & HTTP2 server push
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
  -- * Extraneous Types
  Predicate
) where

import Web.App.State
import Web.App.Monad.RouteT

import Control.Concurrent.STM

import Data.List

import Network.Wai
import Network.Wai.HTTP2
import Network.HTTP.Types.Status (status404)

import Debug.Trace

-- TODO: turn WebAppT into something more like:
-- newtype ScottyT e m a = ScottyT { runS :: State (ScottyState e m) a }

-- | Used to determine if a route can handle a request           
type Predicate = Request -> Bool
               
-- |Monad for defining routes & adding middleware.
newtype WebAppT s m a = WebAppT {
  runWebAppT :: [(Predicate, RouteT s m ())]
             -> [Middleware]
             -> (a,[(Predicate, RouteT s m ())],[Middleware])
}

instance (WebAppState s, Functor m) => Functor (WebAppT s m) where
  fmap f m = WebAppT $ \r mw -> let (a, r', mw') = runWebAppT m r mw
                                in (f a, r', mw')

instance (WebAppState s, Monad m) => Applicative (WebAppT s m) where
  pure a = WebAppT $ \r mw -> (a, r, mw)
  -- (<*>) = ap -- TODO look into this
  WebAppT mf <*> WebAppT mx = WebAppT $ \r mw ->
    let ~(f, r', mw')   = mf r mw
        ~(x, r'', mw'') = mx r' mw'
    in   (f x, r'', mw'')

instance (WebAppState s, Monad m) => Monad (WebAppT s m) where
  m >>= k = WebAppT $ \r mw ->
    let ~(a, r', mw')   = runWebAppT m r mw
        ~(b, r'', mw'') = runWebAppT (k a) r' mw'
    in (b, r'', mw'')
  fail msg = WebAppT $ \_ _ -> error msg

-- |Turn a WebAppT computation into a WAI 'Application'.
toApplication :: (WebAppState s, Monad m, Monad n) => TVar s -- ^ initial state
                                                   -> (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
                                                   -> WebAppT s m () -- ^ a web app
                                                   -> n (HTTP2Application, Application) -- ^ resulting 'Application'
toApplication tvar respToIO act = do
  let ~(_,rts,mws) = runWebAppT act [] []
      withMiddleware = foldl (flip ($)) (mkApp tvar rts) mws
      withMiddleware2 = mkApp2 tvar rts
  return (withMiddleware2, withMiddleware)
  where
    routePasses r (p,_) = p r
    mkApp tvar routes req callback = maybe notFound found $ find (routePasses req) routes
      where notFound = callback $ responseLBS status404 [] "Not found."
            found rt = (toResponse tvar (snd rt) req respToIO) >>= callback
    mkApp2 tvar routes req pushFunc = maybe notFound found $ find (routePasses req) routes
      where notFound = respondNotFound []
            found rt = respondIO $ toResponder tvar (snd rt) req respToIO
      
      
-- |Use a middleware
middleware :: (WebAppState s, Monad m) => Middleware -> WebAppT s m ()
middleware m = WebAppT $ \r mw -> ((),r,mw ++ [m])

-- |Define a route
route :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
route p act = WebAppT $ \r mw -> ((),r ++ [(p,act)],mw)