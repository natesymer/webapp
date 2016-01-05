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
) where

import Web.App.State
import Web.App.Monad.RouteT
import Web.App.Path

import Control.Concurrent.STM

import Data.List

import Network.Wai
import Network.Wai.HTTP2
import Network.HTTP.Types.Status (status404)

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
toApplication st respToIO act = do
  let ~(_,rts,mws) = runWebAppT act [] []
      withMiddleware = foldl (flip ($)) (mkApp st rts) mws
      withMiddleware2 = mkApp2 st rts -- TODO add middleware
  return (withMiddleware2, withMiddleware)
  where
    notFoundHeaders = [("Content-Type", "text/plain; charset=utf-8")]
    routePasses r (p,_) = p r
    mkApp tvar routes req callback = maybe notFound found $ find (routePasses req) routes
      where notFound = callback $ responseLBS status404 notFoundHeaders "Not found."
            found rt = toResponse tvar (snd rt) req (LiteralPath []) respToIO >>= callback
    mkApp2 tvar routes req pushFunc = maybe notFound (found pushFunc) $ find (routePasses req) routes
      where notFound    = respond status404 notFoundHeaders $ streamBuilder "Not found."
            found pf rt = respondIO $ toResponder tvar (snd rt) routes req pf (LiteralPath []) respToIO
      
-- |Use a middleware
middleware :: (WebAppState s, Monad m) => Middleware -> WebAppT s m ()
middleware m = WebAppT $ \r mw -> ((),r,mw ++ [m])

-- |Define a route
route :: (WebAppState s, Monad m) => Predicate -> Path -> RouteT s m () -> WebAppT s m ()
route p pth act = WebAppT $ \r mw -> ((),r ++ [(p,pth,act)],mw)