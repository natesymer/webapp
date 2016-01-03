{-|
Module      : Web.App
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Root modules of webapp.
-}

module Web.App
(
  -- * Functions
  webappMain,
  webappMainIO,
  -- * Exported Modules
  module Web.App.Cookie,
  module Web.App.FileCache,
  module Web.App.HTTP,
  module Web.App.Monad,
  module Web.App.Middleware,
  module Web.App.RouteMatching,
  module Web.App.State
)
where

import Web.App.Cookie
import Web.App.FileCache
import Web.App.HTTP
import Web.App.Internal.Daemon
import Web.App.Internal.IO
import Web.App.Internal.TerminalSize
import Web.App.Monad
import Web.App.State
import Web.App.Middleware
import Web.App.RouteMatching

import Control.Monad.IO.Class

import Control.Applicative
import Options.Applicative
import System.Environment (getArgs)

data Cmd
  = StartCommand {
     startCmdDaemonize :: Bool,
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
  
webappMainIO :: (WebAppState s) => WebAppT s IO ()-- ScottyT e (WebAppM s) () -- ^ app to start
                                -> String -- ^ CLI title/description
                                -> Maybe (Parser a) -- ^ extra CLI parser (available under @util@ subcommand)
                                -> (a -> IO ()) -- ^ action to apply to parse result of 'utilParser'
                                -> IO ()
webappMainIO = webappMain id

-- | Read commandline arguments and start app accordingly. When passing an
-- additional CLI parser, it is made available under the @util@ subcommand.
webappMain :: (WebAppState s, MonadIO m) => (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
                                         -> WebAppT s m ()-- ScottyT e (WebAppM s) () -- ^ app to start
                                         -> String -- ^ CLI title/description
                                         -> Maybe (Parser a) -- ^ extra CLI parser (available under @util@ subcommand)
                                         -> (a -> IO ()) -- ^ action to apply to parse result of 'utilParser'
                                         -> IO ()
webappMain runToIO app title utilParser utilf = getArgs >>= getCommandArgs utilParser title >>= processArgs
  where
    processArgs (Right cmd) = f cmd
    processArgs (Left utils) = utilf utils
    f c@(StartCommand True _ _ _ _ _ _ pidPath) = do
      daemonize pidPath $ f $ c { startCmdDaemonize = False }
    f (StartCommand False False port crt key out err _) = do
      redirectStdout out
      redirectStderr err
      startHTTPS app runToIO port crt key
    f (StartCommand False True port _ _ out err _) = do
      redirectStdout out
      redirectStderr err
      startHTTP app runToIO port
    f (StopCommand pidPath) = daemonKill 4 pidPath
    f (StatusCommand pidPath) = daemonRunning pidPath >>= putStrLn . showStatus
    showStatus True = "running"
    showStatus False = "stopped"

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