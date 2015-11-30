module Web.App
(
  webappMain,
  module Web.App.Assets,
  module Web.App.Cookie,
  module Web.App.Daemon,
  module Web.App.FileCache,
  module Web.App.HTTP,
  module Web.App.Password,
  module Web.App.IO,
  module Web.App.Monad
)
where

import Web.App.Assets
import Web.App.Cookie
import Web.App.Daemon
import Web.App.FileCache
import Web.App.HTTP
import Web.App.Password
import Web.App.IO
import Web.App.Monad
import Web.App.TerminalSize

import Control.Applicative
import Options.Applicative
import System.Environment (getArgs)
import Web.Scotty.Trans (ScottyT,ScottyError)
import System.IO

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
  | PasswordCommand {
    _passwordCmdPassword :: String
  }

webappMain :: (ScottyError e, WebAppState s) => ScottyT e (WebAppM s) () -> String -> String -> IO ()
webappMain app title desc = getArgs >>= getCommandArgs title desc >>= f
  where
    f c@(StartCommand True _ _ _ _ _ _ pidPath) = do
      daemonize pidPath $ f $ c { startCmdDaemonize = False }
    f (StartCommand False False port crt key out err _) = do
      redirectStdout out
      redirectStderr err
      startHTTPS app port crt key
    f (StartCommand False True port _ _ out err _) = do
      redirectStdout out
      redirectStderr err
      startHTTP app port
    f (StopCommand pidPath) = daemonKill 4 pidPath
    f (StatusCommand pidPath) = daemonRunning pidPath >>= putStrLn . showStatus
    f (PasswordCommand pwd) = hashPassword pwd >>= g
      where
        g (Just v) = putStrLn v
        g Nothing = hPutStrLn stderr "failed to hash password"
    showStatus True = "running"
    showStatus False = "stopped"

getCommandArgs :: String -> String -> [String] -> IO Cmd
getCommandArgs title desc args = do
  w <- maybe 80 snd <$> getTermSize
  handleParseResult $ execParserPure (pprefs w) parser args
  where
    pprefs = ParserPrefs "" False False True
    parser = info (helper <*> parseCommand) (fullDesc <> progDesc desc <> header title)
    
parseCommand :: Parser Cmd
parseCommand = sp <|> parseStart
  where
    sp = subparser $ (mkcmd "start" "Start the application server" parseStart) <>
                     (mkcmd "stop" "Stop the application server" parseStop) <>
                     (mkcmd "status" "Determine if the application server is running" parseStatus) <>
                     (mkcmd "password" "Make a BCrypt hash from a password" parseHash)
    parseStart = StartCommand
      <$> (flag False True $ short 'd' <> long "daemonize" <> help "run the application server daemonized")
      <*> (flag False True $ short 'i' <> long "insecure" <> help "run the application server over insecure HTTP")
      <*> (option auto $ opt "port" 'p' "PORT" (Just 3000) "port to run the application server on")
      <*> (strOption $ opt "https-crt" 'c' "FILEPATH" (Just "server.crt") "SSL certificate file")
      <*> (strOption $ opt "https-key" 'k' "FILEPATH" (Just "server.key") "SSL private key file")
      <*> (optional $ strOption $ opt "stdout" 'o' "FILEPATH" Nothing "redirect standard output to FILEPATH")
      <*> (optional $ strOption $ opt "stderr" 'e' "FILEPATH" Nothing "redirect standard error to FILEPATH")
      <*> (strOption $ opt "pid-file" 'z' "FILEPATH" (Just "/tmp/webapp.pid") "when daemonizing, write the PID to FILEPATH")
    parseStop     = StopCommand <$> (strOption $ opt "pid-file" 'z' "FILEPATH" (Just "/tmp/webapp.pid") "pid file")
    parseStatus   = StatusCommand <$> (strOption $ opt "pid-file" 'z' "FILEPATH" (Just "/tmp/webapp.pid") "pid file")
    parseHash     = PasswordCommand <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    opt lng shrt mvar (Just defVal) hlp = (long lng <> short shrt <> metavar mvar <> value defVal <> help hlp)
    opt lng shrt mvar Nothing       hlp = (long lng <> short shrt <> metavar mvar <> help hlp)
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc