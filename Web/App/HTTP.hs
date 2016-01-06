{-# LANGUAGE OverloadedStrings, RankNTypes #-}

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
import Web.App.State

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM

import Network.Wai (Application,Middleware)
import Network.Wai.HTTP2 (HTTP2Application)
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
                                        -> (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
                                        -> Int -- ^ Port to which to bind
                                        -> IO ()
startHTTP app runToIO port = serveApp (runHTTP2Settings) runToIO app port [gzip 860]

-- |Start a secure HTTPS server. Please note that most HTTP/2-compatible browswers
-- require HTTPS in order to upgrade to HTTP/2.
startHTTPS :: (WebAppState s, MonadIO m) => WebAppT s m () -- ^ Scotty app to serve
                                         -> (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
                                         -> Int -- ^ Port to which to bind
                                         -> FilePath -- ^ 'FilePath' to an SSL certificate
                                         -> FilePath -- ^ 'FilePath' to an SSL private key
                                         -> IO ()
startHTTPS app runToIO port cert key = serveApp serve runToIO app port mw
  where mw = [gzip 860, forceSSL]
        serve = runHTTP2TLSHandled tlsset
        tlsset = defaultTlsSettings { keyFile = key, certFile = cert, onInsecure = AllowInsecure }     

{- Internal -}

serveApp :: (WebAppState s, MonadIO m) => (Settings -> HTTP2Application -> Application -> IO ()) -- ^ fnc to serve resulting app
                                       -> (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
                                       -> WebAppT s m () -- ^ app to serve
                                       -> Int -- ^ port to serve on
                                       -> [Middleware] -- ^ middleware to add to the app
                                       -> IO ()
serveApp serve runToIO app port middlewares = do
  (wai2, wai, st) <- toApplication runToIO $ do
    mapM_ middleware middlewares
    app
  serve (warpSettings st) wai2 wai
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
runHTTP2TLSHandled :: TLSSettings -> Settings -> HTTP2Application -> Application -> IO ()
runHTTP2TLSHandled tset set wai2 wai = liftIO $ withSocketsDo $
  bracket (bindPortTCP (getPort set) (getHost set)) sClose $ \socket -> do
    settingsInstallShutdownHandler set (sClose socket)
    runHTTP2TLSSocket tset set socket wai2 wai