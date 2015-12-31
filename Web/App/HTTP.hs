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
import Web.App.Middleware
import Web.App.Internal.Privileges

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM

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
startHTTP :: (WebAppState s, MonadIO m) => WebAppT s m () -- ^ Scotty app to serve
                                        -> Int -- ^ Port to which to bind
                                        -> m ()
startHTTP app port = serveApp serve app port [gzip 860]
  where serve s a2 a = liftIO $ runHTTP2Settings s a2 a
-- |Start a secure HTTPS server. Please note that most HTTP/2-compatible browswers
-- require HTTPS in order to upgrade to HTTP/2.
startHTTPS :: (WebAppState s, MonadIO m) => WebAppT s m () -- ^ Scotty app to serve
                                         -> Int -- ^ Port to which to bind
                                         -> FilePath -- ^ 'FilePath' to an SSL certificate
                                         -> FilePath -- ^ 'FilePath' to an SSL private key
                                         -> m ()
startHTTPS app port cert key = do
  serveApp (runHTTP2TLSHandled tlsset) app port [gzip 860, forceSSL]
  where tlsset = defaultTlsSettings { keyFile = key, certFile = cert, onInsecure = AllowInsecure }
          

{- Internal -}

serveApp :: (WebAppState s, MonadIO m) => (Settings -> HTTP2Application -> Application -> m ()) -- ^ fnc to serve resulting app
                                       -> WebAppT s m () -- ^ app to serve
                                       -> Int -- ^ port to serve on
                                       -> [Middleware]
                                       -> m ()
serveApp serve app port middlewares = do
  st <- liftIO $ newTVarIO =<< initState-- (WebApp <$> (newFileCache "assets/") <*> initState)-- webapp
  wai <- toApplication st $ do
    mapM_ middleware middlewares
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
      void $ killSockets
      readTVarIO tvar >>= destroyState
      exitImmediately ExitSuccess

-- | Serve an HTTP2 application over TLS, obeying 'settingsInstallShutdownHandler'.
-- This setting is ignored in WarpTLS due to a bug (gasp!). See
-- https://github.com/yesodweb/wai/issues/483
runHTTP2TLSHandled :: (MonadIO m) => TLSSettings -> Settings -> HTTP2Application -> Application -> m ()
runHTTP2TLSHandled tset set wai2 wai = liftIO $ withSocketsDo $
  bracket (bindPortTCP (getPort set) (getHost set)) sClose $ \socket -> do
    settingsInstallShutdownHandler set (sClose socket)
    runHTTP2TLSSocket tset set socket wai2 wai