module System.WebApp.IO
(
  redirectStdout,
  redirectStderr,
  redirectStdin
)
where

import Control.Monad (when,void)

import System.IO
import System.Posix

-- IMPORTANT:
--
-- redirectStd* will disable buffering after
-- it swaps file handles because buffering
-- causes funny behavior with writing to files
-- (buffering works fine with terminals/consoles)

redirectStdout :: Maybe FilePath -> IO ()
redirectStdout Nothing = return ()
redirectStdout (Just path) = do
  swapFd stdOutput path
  hSetBuffering stdout NoBuffering

redirectStderr :: Maybe FilePath -> IO ()
redirectStderr Nothing = return ()
redirectStderr (Just path) = do
  swapFd stdError path
  hSetBuffering stderr NoBuffering

redirectStdin :: Maybe FilePath -> IO ()
redirectStdin Nothing = return ()
redirectStdin (Just path) = do
  swapFd stdInput path
  hSetBuffering stdin NoBuffering

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