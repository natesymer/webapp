{-# LANGUAGE OverloadedStrings, RankNTypes #-}

{-|
Module      : Web.App.HTTP
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX
-}

module Web.App.HTTP
(
  runServer
)
where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLSSocket, TLSSettings(..), OnInsecure(..))
import Network.Socket (close, listen, bind, socket, Socket, SocketOption(..), AddrInfoFlag(..), SocketType(..), Family(..), isListening, setSocketOption, maxListenQueue, withSocketsDo, getAddrInfo, defaultHints, addrFamily, addrFlags, addrAddress, addrProtocol, addrSocketType)
import Control.Exception (bracket, bracketOnError)

import System.Exit
import System.Posix

import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Bool (bool)

-- | Serve your application.
runServer :: Maybe (FilePath, FilePath) -- ^ Certificate pair to use (cert, key)
          -> Int -- ^ Port to bind to
          -> IO () -- ^ IO to be performed immediately after binding.
          -> IO () -- ^ IO to be performed on shutdown.
          -> Application -- ^ WAI 'Application'.
          -> IO ()
runServer cp port pre teardown app = withSocketsDo $ bracket (bindTCP port) close logic
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

-- | Find a Socket capable of transmitting/receiving TCP data.
-- Has preference for IPv4 hosts over IPv6 hosts.
bindTCP :: Int -> IO Socket
bindTCP p = tryAddrs . sortOn sortf =<< getAddrInfo (Just hints) Nothing (Just $ show p)
  where hints = defaultHints {
                  addrFlags = [AI_PASSIVE, AI_NUMERICSERV, AI_NUMERICHOST],
                  addrSocketType = Stream
                } -- TODO: add port to these hints somehow
        sortf = (==) AF_INET6 . addrFamily
        tryAddrs = fmap fromJust . findMLazy isListening . map theBody -- uses laziness
        theBody addr = bracketOnError aquire close action
          where aquire = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                action sock = do
                  setSocketOption sock NoDelay 1
                  setSocketOption sock ReuseAddr 1
                  bind sock $ addrAddress addr
                  listen sock $ max 2048 maxListenQueue
                  return sock
      
-- | Performs monadic computations until it finds a result
-- for which p returns true. It returns this value.           
findMLazy :: (Monad m) => (a -> m Bool) -> [m a] -> m (Maybe a)
findMLazy _ [] = return Nothing
findMLazy p (x:xs) = x >>= \x' -> p x' >>= bool (findMLazy p xs) (return $ Just x') 
