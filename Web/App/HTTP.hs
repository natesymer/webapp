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
  runServer
)
where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLSSocket, TLSSettings(..), OnInsecure(..))
import Network.Socket (close, withSocketsDo)
import Data.Streaming.Network (bindPortTCP)
import Control.Exception (bracket)

import System.Exit
import System.Posix

-- | Serve your application.
runServer :: Maybe (FilePath, FilePath) -- ^ Certificate pair to use (cert, key)
          -> Int -- ^ Port to bind to
          -> IO () -- ^ IO to be performed immediately after binding.
          -> IO () -- ^ IO to be performed on shutdown.
          -> Application -- ^ WAI 'Application'.
          -> IO ()
runServer cp port pre teardown app = withSocketsDo $ bracket (bind port) close logic
  where
    logic sock = pre >> do
      installHandler sigTERM handler Nothing
      installHandler sigINT handler Nothing
      run cp sock
      where handler = Catch $ close sock >> teardown >> exitImmediately ExitSuccess
    run (Just (c, k)) sock = runTLSSocket tset set sock app
      where tset = (tlsSettings c k) { onInsecure = AllowInsecure }
    run _ sock = runSettingsSocket set sock app
    set = setPort port defaultSettings
    bind p = bindPortTCP p "*4" -- TODO: re-implement bindPortTCP
