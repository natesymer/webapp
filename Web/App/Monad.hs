{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

{-|
Module      : Web.App.Monad
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Definition of 'WebAppM' monad & 'WebAppState' typeclass.
-}

module Web.App.Monad
(
  -- * Monads
  WebAppM(..),
  -- * Typeclasses
  WebAppState(..),
  -- * Cache Operations
  getCache,
  -- * State Operations
  getState,
  putState,
  modifyState
) where

import Web.App.FileCache (FileCache)
import Web.App.Monad.Internal (WebApp(..))

import Control.Monad.Reader
import Control.Concurrent.STM

-- |Defines a common interface for an app's state
class WebAppState s where
  -- |Create an app state
  initState :: IO s
  -- |Destroy an app state
  destroyState :: s -- ^ the state to destroy
               -> IO ()

-- |Monad providing a 'FileCache' and an application state
newtype WebAppM s a = WebAppM { runWebAppM :: ReaderT (TVar (WebApp s)) IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader (TVar (WebApp s)))

liftWebAppM :: (MonadTrans t, WebAppState s) => WebAppM s a -> t (WebAppM s) a
liftWebAppM = lift

-- |Get the 'FileCache' embedded in a 'WebAppM' monad
getCache :: (MonadTrans t, WebAppState s) => t (WebAppM s) FileCache
getCache = liftWebAppM $ ask >>= liftIO . fmap webAppCache . readTVarIO

-- |Get the state embedded in a 'WebAppM' monad
getState :: (MonadTrans t, WebAppState s) => t (WebAppM s) s
getState = liftWebAppM $ ask >>= liftIO . fmap webAppState . readTVarIO

-- |Embed a new state in a 'WebAppM' monad
putState :: (MonadTrans t, WebAppState s) => s -- ^ state to embed
                                          -> t (WebAppM s) ()
putState st = liftWebAppM $ do
  tv <- ask
  liftIO $ atomically $ do
    (WebApp cache _) <- readTVar tv
    writeTVar tv (WebApp cache st)

-- |Apply a function to the state embedded in a 'WebAppM' monad
modifyState :: (MonadTrans t, WebAppState s) => (s -> s) -- ^ function to apply to state
                                             -> t (WebAppM s) ()
modifyState f = liftWebAppM $ do
  tv <- ask
  liftIO $ atomically $ do
    (WebApp cache st) <- readTVar tv
    writeTVar tv (WebApp cache (f st))