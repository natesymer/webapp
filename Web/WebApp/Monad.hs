{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module System.WebApp.Monad
(
  WebApp(..),
  WebAppM(..),
  WebAppState(..),
  liftWebAppM,
  getWebApp,
  getCache,
  getState,
  putState
) where

import System.WebApp.FileCache

import Control.Monad.Reader
import Control.Concurrent.STM

class WebAppState s where
  initState :: IO s
  destroyState :: s -> IO ()

data WebApp s = WebApp {
  webAppCache :: FileCache,
  webAppState :: s
}

newtype WebAppM s a = WebAppM { runWebAppM :: ReaderT (TVar (WebApp s)) IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader (TVar (WebApp s)))

liftWebAppM :: (MonadTrans t, WebAppState s) => WebAppM s a -> t (WebAppM s) a
liftWebAppM = lift

getWebApp :: (MonadTrans t, WebAppState s) => t (WebAppM s) (WebApp s)
getWebApp = liftWebAppM $ ask >>= liftIO . readTVarIO

getCache :: (MonadTrans t, WebAppState s) => t (WebAppM s) FileCache
getCache = liftWebAppM $ ask >>= liftIO . fmap webAppCache . readTVarIO

-- TODO: double check
getState :: (MonadTrans t, WebAppState s) => t (WebAppM s) s
getState = liftWebAppM $ ask >>= liftIO . fmap webAppState . readTVarIO

putState :: (MonadTrans t, WebAppState s) => s -> t (WebAppM s) ()
putState st = liftWebAppM $ ask >>= liftIO . atomically . flip writeTvarIO st