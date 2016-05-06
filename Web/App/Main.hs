{-|
Module      : Web.App.Main
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Main functions for running webapps. They provide a CLI interface
to the Web.App.HTTP module. They are designed to be used like

@
module Main where

import Web.App

main :: IO ()
main = webappMain' app "My Application!"
@

-}

module Web.App.Main
(
  webappMain,
  webappMain',
  webappMainIO,
  webappMainIO'
)
where

import Web.App.Middleware
import Web.App.WebApp
import Web.App.RouteT (RouteResult)
import Web.App.State
import Web.App.HTTP
import Web.App.Internal.IO
import Web.App.Internal.Daemon
import Web.App.Internal.TerminalSize

import Control.Monad.IO.Class

import Control.Applicative
import Options.Applicative
import System.Environment (getArgs)
import System.Posix

data Cmd
  = StartCommand {
    _startCmdDaemonize :: Bool,
    _startCmdInsecure :: Bool,
    _startCmdPort :: Int,
    _startCmdHTTPSSLCert :: FilePath,
    _startCmdHTTPSSLKey :: FilePath,
    _startCmdOutputPath :: Maybe FilePath,
    _startCmdErrorPath :: Maybe FilePath,
    _startCmdPidPath :: FilePath
  }
  | StopCommand {
    _stopCmdPidPath :: FilePath
  }
  | StatusCommand {
    _statusCmdPidPath :: FilePath
  }
  
-- |Like 'webappMainIO' without the CLI extension arguments.
webappMainIO' :: (WebAppState s)
              => WebApp s IO -- ^ app to start
              -> String -- ^ CLI title/description
              -> IO ()
webappMainIO' a d = webappMainIO a d Nothing (const $ return ())
  
-- |Run a webapp based on IO.
webappMainIO :: (WebAppState s)
             => WebApp s IO -- ^ app to start
             -> String -- ^ CLI title/description
             -> Maybe (Parser a) -- ^ extra CLI parser (available under @util@ subcommand)
             -> (a -> IO ()) -- ^ action to apply to parse result of 'utilParser'
             -> IO ()
webappMainIO = webappMain id

-- |Like 'webappMain' without the CLI extension arguments.
webappMain' :: (WebAppState s, MonadIO m)
            => (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
            -> WebApp s m -- ^ app to start
            -> String -- ^ CLI title/description
            -> IO ()
webappMain' f a d = webappMain f a d Nothing (const $ return ())

-- | Read commandline arguments and start webapp accordingly. When passing an
-- additional CLI parser, it is made available under the @util@ subcommand.
webappMain :: (WebAppState s, MonadIO m)
           => (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
           -> WebApp s m -- ^ app to start
           -> String -- ^ CLI title/description
           -> Maybe (Parser a) -- ^ extra CLI parser (available under @util@ subcommand)
           -> (a -> IO ()) -- ^ action to apply to parse result of 'utilParser'
           -> IO ()
webappMain runToIO app title utilParser utilf = getArgs >>= getCommandArgs utilParser title >>= processArgs
  where
    processArgs (Right cmd) = f cmd
    processArgs (Left utils) = utilf utils
    f (StartCommand True i p c k o e pidPath) = daemonize pidPath $ start i p c k o e
    f (StartCommand False i p c k o e _) = start i p c k o e
    f (StopCommand pidPath) = daemonKill 4 pidPath
    f (StatusCommand pidPath) = daemonRunning pidPath >>= putStrLn . showStatus
    showStatus True = "running"
    showStatus False = "stopped"
    start insecure port cert key out err = do
      bindTCP port $ \sock -> do
        -- drop privileges after binding to a port
        getRealGroupID >>= setEffectiveGroupID
        getRealUserID >>= setEffectiveUserID
        -- redirect I/O
        redirectStdout out
        redirectStderr err
        -- serve webapp
        serve sock (if insecure then [gzip 860] else [gzip 860,forceSSL port])
      where serveFunc True = runInsecure
            serveFunc False = runSecure cert key
            serve sock = serveApp (serveFunc insecure sock) runToIO app port

getCommandArgs :: Maybe (Parser a) -> String -> [String] -> IO (Either a Cmd)
getCommandArgs utilParser title args = do
  w <- maybe 80 snd <$> getTermSize
  handleParseResult $ execParserPure (pprefs w) parser args
  where
    pprefs = ParserPrefs "" False False True
    parser = info (helper <*> ((sp utilParser) <|> parseStart)) (fullDesc <> header title)
    sp Nothing = subparser subCommands
    sp (Just util) = subparser $ subCommands <> (mkcmd "util" "Utilities associated with the application" (Left <$> util))
    subCommands = (mkcmd "start" "Start the application server" parseStart) <>
                  (mkcmd "stop" "Stop the application server" parseStop) <>
                  (mkcmd "status" "Determine if the application server is running" parseStatus)
    parseStart = fmap Right $ StartCommand
      <$> (flag False True $ short 'd' <> long "daemonize" <> help "run the application server daemonized")
      <*> (flag False True $ short 'i' <> long "insecure" <> help "run the application server over insecure HTTP")
      <*> (option auto $ opt "port" 'p' "PORT" (Just 3000) "port to run the application server on")
      <*> (strOption $ opt "https-crt" 'c' "FILEPATH" (Just "server.crt") "SSL certificate file")
      <*> (strOption $ opt "https-key" 'k' "FILEPATH" (Just "server.key") "SSL private key file")
      <*> (optional $ strOption $ opt "stdout" 'o' "FILEPATH" Nothing "redirect standard output to FILEPATH")
      <*> (optional $ strOption $ opt "stderr" 'e' "FILEPATH" Nothing "redirect standard error to FILEPATH")
      <*> (strOption $ opt "pid-file" 'z' "FILEPATH" (Just "/tmp/webapp.pid") "when daemonizing, write the PID to FILEPATH")
    parseStop     = Right <$> StopCommand <$> (strOption $ opt "pid-file" 'z' "FILEPATH" (Just "/tmp/webapp.pid") "pid file")
    parseStatus   = Right <$> StatusCommand <$> (strOption $ opt "pid-file" 'z' "FILEPATH" (Just "/tmp/webapp.pid") "pid file")
    opt lng shrt mvar (Just defVal) hlp = (long lng <> short shrt <> metavar mvar <> value defVal <> help hlp)
    opt lng shrt mvar Nothing       hlp = (long lng <> short shrt <> metavar mvar <> help hlp)
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc