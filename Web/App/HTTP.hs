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
import Web.App.FileCache
import Web.App.Middleware
import Web.App.Privileges

import Control.Monad
import Control.Monad.Reader (runReaderT)
import Control.Concurrent.STM

import Web.Scotty.Trans as Scotty

import Network.Wai (Application,Middleware)
import Network.Wai.HTTP2 (promoteApplication,HTTP2Application)
import Network.Wai.Handler.Warp (defaultSettings,setPort,getPort,getHost,setBeforeMainLoop,setInstallShutdownHandler,runHTTP2Settings)
import Network.Wai.Handler.WarpTLS (certFile,defaultTlsSettings,keyFile,TLSSettings(..),runHTTP2TLSSocket,OnInsecure(..))
import Network.Wai.Handler.Warp.Internal (Settings(..))
import Network.Socket (sClose, withSocketsDo)
import Data.Streaming.Network (bindPortTCP)
import Control.Exception (bracket)

import System.Exit
import System.Posix

-- |Start an insecure HTTP server.
startHTTP :: (ScottyError e, WebAppState s) => ScottyT e (WebAppM s) () -- ^ Scotty app to serve
                                            -> Int -- ^ Port to which to bind
                                            -> IO ()
startHTTP app port = serveApp runHTTP2Settings app port [gzip 860]

-- TODO: fix setInstallShutdownHandler not working
-- |Start a secure HTTPS server. Please note that most HTTP/2-compatible browswers
-- require HTTPS in order to upgrade to HTTP/2.
startHTTPS :: (ScottyError e, WebAppState s) => ScottyT e (WebAppM s) () -- ^ Scotty app to serve
                                             -> Int -- ^ Port to which to bind
                                             -> FilePath -- ^ 'FilePath' to an SSL certificate
                                             -> FilePath -- ^ 'FilePath' to an SSL private key
                                             -> IO ()
startHTTPS app port cert key = do
  serveApp (runHTTP2TLSHandled tlsset) app port [gzip 860, forceSSL]
  where tlsset = defaultTlsSettings { keyFile = key, certFile = cert, onInsecure = AllowInsecure }
          

{- Internal -}
      
serveApp :: (ScottyError e, WebAppState s) => (Settings -> HTTP2Application -> Application -> IO ()) -- ^ fnc to serve resulting app
                                           -> ScottyT e (WebAppM s) () -- ^ scotty app to serve
                                           -> Int -- ^ port to serve on
                                           -> [Middleware]
                                           -> IO ()
serveApp serve app port middlewares = do
  st <- newTVarIO =<< (WebApp <$> (newFileCache "assets/") <*> initState)-- webapp
  wai <- scottyAppT (\m -> runReaderT (runWebAppM m) st) $ do
    mapM_ middleware middlewares
    middleware $ gzip 860 -- min length to GZIP
    middleware $ forceSSL
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
      exitImmediately ExitSuccess

-- | Serve an HTTP2 application over TLS, obeying 'settingsInstallShutdownHandler'.
-- This setting is ignored in WarpTLS due to a bug (gasp!). See
-- https://github.com/yesodweb/wai/issues/483
runHTTP2TLSHandled :: TLSSettings -> Settings -> HTTP2Application -> Application -> IO ()
runHTTP2TLSHandled tset set wai2 wai = withSocketsDo $
  bracket (bindPortTCP (getPort set) (getHost set)) sClose $ \socket -> do
    settingsInstallShutdownHandler set (sClose socket)
    runHTTP2TLSSocket tset set socket wai2 wai