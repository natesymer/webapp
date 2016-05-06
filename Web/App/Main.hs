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

{-# OPTIONS -fno-warn-unused-do-bind #-}

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
import Web.App.Internal.TerminalSize

import Control.Monad.IO.Class

import Control.Applicative
import Options.Applicative
import System.Posix
import System.Exit

data Cmd
  = StartCommand {
    _startCmdDaemonize :: Maybe FilePath,
    _startCmdInsecure :: Bool,
    _startCmdPort :: Int,
    _startCmdHTTPSSLCert :: FilePath,
    _startCmdHTTPSSLKey :: FilePath,
    _startCmdOutputPath :: Maybe FilePath,
    _startCmdErrorPath :: Maybe FilePath
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
           -> Maybe (Parser a) -- ^ extra CLI parser, parsed after the built-in parser
           -> (a -> IO ()) -- ^ action to apply the result of 'extraParser'
           -> IO ()
webappMain runToIO app title extraParser extraf = parseArgs extraParser title >>= either extraf f
  where
    f (StartCommand Nothing i p c k o e) = start i p c k o e
    f (StartCommand (Just pidFile) i p c k o e) = do
      forkProcess $ do
        createSession
        forkProcess $ do
          getProcessID >>= writeFile pidFile . show
          redirectStdout $ Just "/dev/null"
          redirectStderr $ Just "/dev/null"
          redirectStdin $ Just "/dev/null"
          closeFd stdInput -- close STDIN
          installHandler sigHUP Ignore Nothing
          start i p c k o e
        exitImmediately ExitSuccess
      exitImmediately ExitSuccess
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

parseArgs :: Maybe (Parser a) -> String -> IO (Either a Cmd)
parseArgs extraParser title = do
  w <- maybe 80 snd <$> getTermSize
  customExecParser (mkprefs w) parser
  where
    mkprefs = ParserPrefs "" False False True
    mkparser = (<|>) (Right <$> parseStart) . maybe empty (fmap Left)
    parseStart = StartCommand
      <$> (optional $ strOption $ long "daemonize" <> short 'd' <> metavar "FILEPATH" <> help "daemonize server and write its pid to FILEPATH")
      <*> (flag False True $ short 'i' <> long "insecure" <> help "run server over insecure HTTP")
      <*> (option auto $ long "port" <> short 'p' <> metavar "PORT" <> value 3000 <> help "run server on PORT")
      <*> (strOption $ long "cert" <> short 'c' <> metavar "FILEPATH" <> value "server.crt" <> help "SSL certificate file")
      <*> (strOption $ long "key" <> short 'k' <> metavar "FILEPATH" <> value "server.key" <> help "SSL private key file")
      <*> (optional $ strOption $ long "stdout" <> short 'o' <> metavar "FILEPATH" <> help "redirect output to FILEPATH")
      <*> (optional $ strOption $ long "stderr" <> short 'e' <> metavar "FILEPATH" <> help "redirect error to FILEPATH")
    parser = info (helper <*> mkparser extraParser) (fullDesc <> header title)