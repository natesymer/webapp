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

import Web.App.WebApp
import Web.App.RouteT (RouteResult)
import Web.App.State
import Web.App.HTTP

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class

import Control.Applicative
import Options.Applicative
import System.Posix
import System.Exit
import System.Environment
import System.IO

import GHC.IO.Handle

import Text.Read

data Options = Options {
  _optionsDaemonize :: Maybe FilePath,
  _optionsPort :: Int,
  _optionsHTTPSSLCert :: Maybe FilePath,
  _optionsHTTPSSLKey :: Maybe FilePath,
  _optionsOutputPath :: Maybe FilePath,
  _optionsErrorPath :: Maybe FilePath
}

-- |Like 'webappMainIO' without the CLI extension arguments.
webappMainIO' :: (WebAppState s)
              => WebApp s IO -- ^ app to start
              -> IO ()
webappMainIO' a = webappMainIO a Nothing (const $ return ())
  
-- |Run a webapp based on IO.
webappMainIO :: (WebAppState s)
             => WebApp s IO -- ^ app to start
             -> Maybe (Parser a) -- ^ extra CLI parser (available under @util@ subcommand)
             -> (a -> IO ()) -- ^ action to apply to parse result of 'utilParser'
             -> IO ()
webappMainIO = webappMain id

-- |Like 'webappMain' without the CLI extension arguments.
webappMain' :: (WebAppState s, MonadIO m)
            => (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
            -> WebApp s m -- ^ app to start
            -> IO ()
webappMain' f a = webappMain f a Nothing (const $ return ())

-- | Read commandline arguments and start webapp accordingly. When passing an
-- additional CLI parser, it is made available under the @util@ subcommand.
webappMain :: (WebAppState s, MonadIO m)
           => (m RouteResult -> IO RouteResult) -- ^ action to eval a monadic computation in @m@ in @IO@
           -> WebApp s m -- ^ app to start
           -> Maybe (Parser a) -- ^ extra CLI parser, parsed after the built-in parser
           -> (a -> IO ()) -- ^ action to apply the result of 'extraParser'
           -> IO ()
webappMain runToIO app extraParser extraf = parseArgs extraParser >>= either extraf f
  where
    f (Options Nothing p c k o e) = start p c k o e
    f (Options (Just pidFile) p c k o e) = do
      forkProcess $ do
        createSession
        forkProcess $ do
          getProcessID >>= writeFile pidFile . show
          redirectHandle stdout $ fromMaybe "/dev/null" o
          redirectHandle stderr $ fromMaybe "/dev/null" e
          redirectHandle stdin "/dev/null"
          installHandler sigHUP Ignore Nothing
          start p c k Nothing Nothing
        exitImmediately ExitSuccess
      exitImmediately ExitSuccess
    start port cert key out err = do
      bindTCP port $ \sock -> do
        -- drop privileges after binding to a port
        getRealGroupID >>= setEffectiveGroupID
        getRealUserID >>= setEffectiveUserID
        -- redirect I/O
        maybe (return ()) (redirectHandle stdout) out
        maybe (return ()) (redirectHandle stderr) err
        -- serve webapp
        (wai,teardown) <- toApplication runToIO app
        serveFunc cert key sock (mkWarpSettings teardown port) wai
      where serveFunc c k = fromMaybe runInsecure $ runSecure <$> c <*> k
    redirectHandle hdl path = do
      exists <- fileExist path
      when (not exists) $ writeFile path ""
      h <- openFile path WriteMode
      hDuplicateTo h hdl
      hClose h
      hSetBuffering hdl NoBuffering
      
parseArgs :: Maybe (Parser a) -> IO (Either a Options)
parseArgs extra = do
  defaultPort <- ((=<<) readMaybe) <$> lookupEnv "PORT"
  customExecParser pprefs $ info (helper <*> parser defaultPort) fullDesc
  where
    pprefs = ParserPrefs "" False False True 80
    parser port = (Right <$> parseStart port) <|> (maybe empty (fmap Left) extra)--  <|> (Right <$> showHelp)
    parseStart port = Options
      <$> (optional $ strOption $ long "daemonize"  <> short 'd' <> metavar "FILEPATH" <> help "Daemonize server and write its pid to FILEPATH.")
      <*> (option auto $          long "port"       <> short 'p' <> metavar "PORT"     <> help "Run server on PORT." <> value (fromMaybe 3000 port))
      <*> (optional $ strOption $ long "ssl-cert"   <> short 'c' <> metavar "FILEPATH" <> help "SSL certificate file. If a certificate and key are provided, the server will be run secure.")
      <*> (optional $ strOption $ long "ssl-key"    <> short 'k' <> metavar "FILEPATH" <> help "SSL private key file. If a certificate and key are provided, the server will be run secure.")
      <*> (optional $ strOption $ long "output-log" <> short 'o' <> metavar "FILEPATH" <> help "Redirect output to FILEPATH.")
      <*> (optional $ strOption $ long "error-log"  <> short 'e' <> metavar "FILEPATH" <> help "Redirect error to FILEPATH.")