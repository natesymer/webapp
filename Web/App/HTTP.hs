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
import Network.Wai.Handler.Warp (defaultSettings,setPort,getPort,getHost,setBeforeMainLoop,setInstallShutdownHandler,runHTTP2Settings)
import Network.Wai.Handler.WarpTLS (certFile,defaultTlsSettings,keyFile,TLSSettings(..),runHTTP2TLSSocket)
import Network.Wai.Handler.Warp.Internal (Settings(..))
import Network.HTTP.Types.Status (status301)
import Network.Socket (sClose, withSocketsDo)
import Data.Streaming.Network (bindPortTCP)
import Control.Exception (bracket)

import System.Exit
import System.Posix

-- |Start an insecure HTTP server.
startHTTP :: (ScottyError e, WebAppState s) => ScottyT e (WebAppM s) () -- ^ Scotty app to serve
                                            -> Int -- ^ Port to which to bind
                                            -> IO ()
startHTTP app port = serveApp runHTTP2Settings app port

-- TODO: fix setInstallShutdownHandler not working
-- |Start a secure HTTPS server. Please note that most HTTP/2-compatible browswers
-- require HTTPS in order to upgrade to HTTP/2.
startHTTPS :: (ScottyError e, WebAppState s) => ScottyT e (WebAppM s) () -- ^ Scotty app to serve
                                             -> Int -- ^ Port to which to bind
                                             -> FilePath -- ^ 'FilePath' to an SSL certificate
                                             -> FilePath -- ^ 'FilePath' to an SSL private key
                                             -> IO ()
startHTTPS app port cert key = do
  whenPrivileged startRedirectProcess
  serveApp (runHTTP2TLSHandled tlsset) app port
  where tlsset = defaultTlsSettings { keyFile = key, certFile = cert }
          

{- Internal -}
      
serveApp :: (ScottyError e, WebAppState s) => (Settings -> HTTP2Application -> Application -> IO ()) -- ^ function to serve resulting app
                                           -> ScottyT e (WebAppM s) () -- ^ scotty app to serve
                                           -> Int -- ^ port to serve on
                                           -> IO ()
serveApp serve app port = do
  st <- newTVarIO =<< (WebApp <$> (newFileCache "assets/") <*> initState)-- webapp
  wai <- scottyAppT (\m -> runReaderT (runWebAppM m) st) $ do
    middleware $ gzip 860 -- min length to GZIP
    app
  serve (warpSettings st) (promoteApplication wai) wai
  where
    warpSettings tvar = setBeforeMainLoop before
                        $ setInstallShutdownHandler (installShutdown tvar)
                        $ setPort port defaultSettings
    before = resignPrivileges "daemon"
    installShutdown tvar killSockets = do
      void $ installHandler sigTERM (handler tvar killSockets) Nothing
      void $ installHandler sigINT (handler tvar killSockets) Nothing
    handler tvar killSockets = Catch $ do
      (WebApp cache st) <- readTVarIO tvar
      void $ killSockets
      teardownFileCache cache
      destroyState st

-- | Serve an HTTP2 application over TLS, obeying 'settingsInstallShutdownHandler'.
-- This setting is ignored in WarpTLS due to a bug (gasp!). See
-- https://github.com/yesodweb/wai/issues/483
runHTTP2TLSHandled :: TLSSettings -> Settings -> HTTP2Application -> Application -> IO ()
runHTTP2TLSHandled tset set wai2 wai = withSocketsDo $
  bracket (bindPortTCP (getPort set) (getHost set)) sClose $ \socket -> do
      settingsInstallShutdownHandler set (sClose socket)
      runHTTP2TLSSocket tset set socket wai2 wai

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
      