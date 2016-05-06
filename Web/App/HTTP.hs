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
  -- * Running Warp
  serveApp,
  runSecure,
  runInsecure,
  -- * Bind to a TCP port for HTTP
  bindTCP
)
where


import Web.App.RouteT
import Web.App.State
import Web.App.WebApp

import Control.Monad
import Control.Monad.IO.Class

import Network.Wai (Application,Middleware)
import Network.Wai.Handler.Warp (defaultSettings,runSettingsSocket,Port)
import Network.Wai.Handler.WarpTLS (tlsSettings,runTLSSocket,TLSSettings(..),OnInsecure(..))
import Network.Wai.Handler.Warp.Internal
import Network.Socket (sClose,withSocketsDo,Socket)
import Data.Streaming.Network (bindPortTCP)
import Control.Exception (bracket)

import System.Exit
import System.Posix

-- TODO: sanity checks for being able to bind to a given port
bindTCP :: Port -> (Socket -> IO ()) -> IO ()
bindTCP port f = withSocketsDo $ bracket (bindPortTCP port "*4") sClose f

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
  (wai,teardown) <- toApplication runToIO $ mconcat $ app:map middleware mws
  serve (warpSettings teardown) wai
  where
    warpSettings td = defaultSettings {
      settingsHTTP2Enabled = True, -- explicitly enable HTTP2 support
      settingsPort = port,
      settingsInstallShutdownHandler = \killSockets -> do
        void $ installHandler sigTERM (handler $ killSockets >> td) Nothing
        void $ installHandler sigINT (handler $ killSockets >> td) Nothing
    }
    handler = Catch . (>> exitImmediately ExitSuccess)
    
-- |Serve a WAI app using Warp over TLS.
runSecure :: FilePath -- ^ 'FilePath' to an SSL certificate
          -> FilePath -- ^ 'FilePath' to an SSL private key
          -> Socket -- ^ Socket to use
          -> Settings -- ^ Warp settings structure
          -> Application -- ^ WAI application
          -> IO ()
runSecure cert key sock set app = do
  settingsInstallShutdownHandler set (sClose sock) -- ignored by Warp's @runTLSSocket@
  runTLSSocket tset set sock app
  where tset = (tlsSettings cert key) { onInsecure = AllowInsecure }

-- |Serve a WAI app using Warp over unencrypted HTTP.
runInsecure :: Socket -- ^ Socket to use
            -> Settings -- ^ Warp settings structure
            -> Application -- ^ WAI application
            -> IO ()
runInsecure sock set = runSettingsSocket set sock