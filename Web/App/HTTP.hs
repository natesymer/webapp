{-# LANGUAGE OverloadedStrings #-}

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

import Web.App.Monad
import Web.App.Monad.Internal
import Web.App.IO
import Web.App.FileCache
import Web.App.Gzip
import Web.App.Privileges

import Data.Maybe
import Control.Monad
import Control.Monad.Reader (runReaderT)
import Control.Concurrent.STM

import Web.Scotty.Trans as Scotty

import Network.Wai (responseLBS,requestHeaderHost,rawPathInfo,rawQueryString,Application)
import Network.Wai.HTTP2 (promoteApplication,HTTP2Application)
import Network.Wai.Handler.Warp (defaultSettings,setPort,setBeforeMainLoop,setInstallShutdownHandler,runHTTP2Settings,Settings)
import Network.Wai.Handler.WarpTLS (certFile,defaultTlsSettings,keyFile,runHTTP2TLS)
import Network.HTTP.Types.Status (status301)
import Network.Wai.Middleware.AddHeaders

import System.Exit
import System.Posix

-- |Start an insecure HTTP server.
startHTTP :: (ScottyError e, WebAppState s) => ScottyT e (WebAppM s) () -- ^ Scotty app to serve
                                            -> Int -- ^ Port to which to bind
                                            -> IO ()
startHTTP app port = do
  (wai,wai2,webapp) <- mkApplication app False
  runHTTP2Settings (mkWarpSettings port webapp) wai2 wai

-- |Start a secure HTTPS server. Please note that most HTTP/2-compatible browswers
-- require HTTPS in order to upgrade to HTTP/2.
startHTTPS :: (ScottyError e, WebAppState s) => ScottyT e (WebAppM s) () -- ^ Scotty app to serve
                                             -> Int -- ^ Port to which to bind
                                             -> FilePath -- ^ 'FilePath' to an SSL certificate
                                             -> FilePath -- ^ 'FilePath' to an SSL private key
                                             -> IO ()
startHTTPS app port cert key = do
  whenPrivileged startRedirectProcess
  (wai,wai2,webapp) <- mkApplication app True
  runHTTP2TLS tlssettings (mkWarpSettings port webapp) wai2 wai
  where tlssettings = defaultTlsSettings { keyFile = key, certFile = cert }

{- Internal -}

mkWarpSettings :: (WebAppState s) => Int -> WebApp s -> Settings
mkWarpSettings port (WebApp cache st) = setBeforeMainLoop before
                            $ setInstallShutdownHandler shutdown
                            $ setPort port
                            defaultSettings
  where
    before = resignPrivileges "daemon"
    shutdown act = do
      void $ act
      teardownFileCache cache
      destroyState st
      
mkApplication :: (ScottyError e, WebAppState s) => ScottyT e (WebAppM s) () -> Bool -> IO (Application, HTTP2Application, WebApp s)
mkApplication app ssl = do
  webapp <- WebApp <$> (newFileCache "assets/") <*> initState
  sync <- newTVarIO webapp
  let runActionToIO m = runReaderT (runWebAppM m) sync
  wai <- scottyAppT runActionToIO $ do
    middleware $ gzip 860 -- min length to GZIP
    when ssl $ middleware $ addHeaders [("Strict-Transport-Security","max-age=31536000")]
    app
  return (wai, promoteApplication wai, webapp)

startRedirectProcess :: IO ()
startRedirectProcess = void $ do
  putStrLn "starting HTTP -> HTTPS process"
  -- TODO: improve separation from parent process
  pid <- forkProcess $ do
    redirectStdout $ Just "/dev/null"
    redirectStderr $ Just "/dev/null"
    redirectStdin $ Just "/dev/null"
    void $ installHandler sigTERM (Catch childHandler) Nothing
    runHTTP2Settings warpSettings (promoteApplication app) app
      
  void $ installHandler sigTERM (Catch $ parentHandler pid) Nothing
  void $ installHandler sigINT (Catch $ parentHandler pid) Nothing
  where
    warpSettings = setBeforeMainLoop (resignPrivileges "daemon") $ setPort 80 defaultSettings
    mkHeaders r = [("Location", url r)]
    host = fromJust . requestHeaderHost
    url r = mconcat ["https://", host r, rawPathInfo r, rawQueryString r]
    childHandler = exitImmediately ExitSuccess
    parentHandler pid = do
      putStrLn "killing HTTP -> HTTPS process"
      signalProcess sigTERM pid
      exitImmediately ExitSuccess
    app req respond = respond $ responseLBS status301 (mkHeaders req) ""
      