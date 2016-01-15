{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}

{-|
Module      : Web.App.HTTP
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Start an HTTP server.
-}

module Web.App.HTTP
(
  -- * Insecure HTTP
  startHTTP,
  -- * Secure HTTPS
  startHTTPS
)
where

import Web.App.Middleware
import Web.App.Internal.Privileges
import Web.App.RouteT
import Web.App.State
import Web.App.WebApp

import Control.Monad
import Control.Monad.IO.Class

import Network.Wai (Application,Middleware)
import Network.Wai.Handler.Warp (defaultSettings,getPort,getHost,runSettings)
import Network.Wai.Handler.WarpTLS (tlsSettings,runTLSSocket,TLSSettings(..),OnInsecure(..))
import Network.Wai.Handler.Warp.Internal -- (Settings(..))
import Network.Socket (sClose, withSocketsDo)
import Data.Streaming.Network (bindPortTCP)
import Control.Exception (bracket)

import System.Exit
import System.Posix

-- |Start an insecure HTTP server.
startHTTP :: (WebAppState s, MonadIO m)
          => WebApp s m -- ^ Scotty app to serve
          -> (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
          -> Int -- ^ Port to which to bind
          -> IO ()
startHTTP app runToIO port = serveApp runSettings runToIO app port [gzip 860]

-- |Start a secure HTTPS server. Please note that most HTTP/2-compatible browswers
-- require HTTPS in order to upgrade to HTTP/2.
startHTTPS :: (WebAppState s, MonadIO m)
           => WebApp s m -- ^ Scotty app to serve
           -> (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
           -> Int -- ^ Port to which to bind
           -> FilePath -- ^ 'FilePath' to an SSL certificate
           -> FilePath -- ^ 'FilePath' to an SSL private key
           -> IO ()
startHTTPS app runToIO port cert key = serveApp (runTLSHandled tlsset) runToIO app port mw
  where mw = [gzip 860, forceSSL]
        tlsset = (tlsSettings cert key) { onInsecure = AllowInsecure }

{- Internal -}

-- |Serves a @'WebAppT' s m ()@ given a serving function, "dropping"
-- function @(m 'RouteResult' -> 'IO' 'RouteResult')@, port, and middlewares.
serveApp :: (WebAppState s, MonadIO m)
         => (Settings -> Application -> IO ()) -- ^ serving function
         -> (m RouteResult -> IO RouteResult) -- ^ "dropping" function
         -> WebApp s m -- ^ app to serve
         -> Int -- ^ port to serve on
         -> [Middleware] -- ^ middleware to add to the app
         -> IO ()
serveApp serve runToIO app port mws = do
  (wai, teardown) <- toApplication runToIO $ mconcat $ app:map middleware mws
  serve (warpSettings teardown) wai
  where
    warpSettings td = defaultSettings {
      settingsPort = port,
      settingsInstallShutdownHandler = installShutdown td,
      settingsBeforeMainLoop = before,
      settingsHTTP2Enabled = True -- explicitly enable HTTP2 support
    }
    before = resignPrivileges "daemon"
    installShutdown teardown killSockets = do
      void $ installHandler sigTERM (handler teardown killSockets) Nothing
      void $ installHandler sigINT (handler teardown killSockets) Nothing
    handler teardown killSockets = Catch $ do
      void $ killSockets
      void $ teardown
      exitImmediately ExitSuccess

-- |Serve an application over TLS, obeying 'settingsInstallShutdownHandler'.
-- This setting is ignored in WarpTLS due to a bug (gasp!). See
-- https://github.com/yesodweb/wai/issues/483
runTLSHandled :: TLSSettings -> Settings -> Application -> IO ()
runTLSHandled tset set wai = withSocketsDo $ bracket acquire destroy f
  where
    acquire = bindPortTCP (getPort set) (getHost set)
    destroy = sClose
    f socket = do
      settingsInstallShutdownHandler set (sClose socket)
      runTLSSocket tset set socket wai