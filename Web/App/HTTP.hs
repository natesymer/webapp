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
  runSecure,
  runInsecure,
  -- * Configuring Warp
  mkWarpSettings,
  -- * Bind to a TCP port for HTTP
  bindTCP
)
where

import Control.Monad

import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings,runSettingsSocket,Port)
import Network.Wai.Handler.WarpTLS (tlsSettings,runTLSSocket,TLSSettings(..),OnInsecure(..))
import Network.Wai.Handler.Warp.Internal
import Network.Socket (close,withSocketsDo,Socket)
import Data.Streaming.Network (bindPortTCP)
import Control.Exception (bracket)

import System.Exit
import System.Posix

bindTCP :: Port -> (Socket -> IO ()) -> IO ()
bindTCP port f = withSocketsDo $ bracket (bindPortTCP port "*4") close f

-- |Build a Warp Settings struct
mkWarpSettings :: IO () -- ^ function to be called on a SIGTERM or SIGINT
               -> Int -- ^ port
               -> Settings
mkWarpSettings teardown port = defaultSettings {
    settingsPort = port,
    settingsInstallShutdownHandler = \killSockets -> void $ do
      installHandler sigTERM (handler $ killSockets >> teardown) Nothing
      installHandler sigINT (handler $ killSockets >> teardown) Nothing
  }
  where handler = Catch . (>> exitImmediately ExitSuccess)
    
-- |Serve a WAI app using Warp over TLS.
runSecure :: FilePath -- ^ 'FilePath' to an SSL certificate
          -> FilePath -- ^ 'FilePath' to an SSL private key
          -> Socket -- ^ Socket to use
          -> Settings -- ^ Warp settings structure
          -> Application -- ^ WAI application
          -> IO ()
runSecure cert key sock set app = do
  settingsInstallShutdownHandler set (close sock) -- ignored by Warp's @runTLSSocket@
  runTLSSocket tset set sock app
  where tset = (tlsSettings cert key) { onInsecure = AllowInsecure }

-- |Serve a WAI app using Warp over unencrypted HTTP.
runInsecure :: Socket -- ^ Socket to use
            -> Settings -- ^ Warp settings structure
            -> Application -- ^ WAI application
            -> IO ()
runInsecure = flip runSettingsSocket