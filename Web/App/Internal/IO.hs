{-|
Module      : Web.App.IO
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Redirect standard input, output, and error. These functions will disable buffering
after they swap file handles because buffering causes funny behavior with respect
to writing to files. (buffering works fine with terminals/consoles).
-}

module Web.App.Internal.IO
(
  -- * IO Redirection
  redirectStdin,
  redirectStdout,
  redirectStderr
)
where

import Control.Monad (when,void)

import System.IO
import System.Posix

-- | Redirect standard input.
redirectStdin :: Maybe FilePath -- ^ File to which standard input is redirected
              -> IO ()
redirectStdin Nothing = return ()
redirectStdin (Just path) = do
  swapFd stdInput path
  hSetBuffering stdin NoBuffering

-- | Redirect standard output.
redirectStdout :: Maybe FilePath -- ^ File to which standard output is redirected
               -> IO ()
redirectStdout Nothing = return ()
redirectStdout (Just path) = do
  swapFd stdOutput path
  hSetBuffering stdout NoBuffering

-- | Redirect standard error.
redirectStderr :: Maybe FilePath -- ^ File to which standard error is redirected
               -> IO ()
redirectStderr Nothing = return ()
redirectStderr (Just path) = do
  swapFd stdError path
  hSetBuffering stderr NoBuffering

{- Internal -}

safeOpenFd :: FilePath -> IO Fd
safeOpenFd p = do
  exists <- fileExist p
  when (not exists) $ writeFile p ""
  fd <- openFd p ReadWrite Nothing defaultFileFlags
  setFdOption fd AppendOnWrite True
  return fd

swapFd :: Fd -> FilePath -> IO ()
swapFd old path = do
  new <- safeOpenFd path
  void $ dupTo new old
  closeFd new