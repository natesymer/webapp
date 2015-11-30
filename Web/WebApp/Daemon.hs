module System.WebApp.Daemon
(
  daemonize,
  daemonRunning,
  daemonKill
)
where
  
import System.WebApp.IO
  
import Control.Exception
import Control.Monad (when,void)

import System.Exit
import System.Posix

daemonKill :: Int -> FilePath -> IO ()
daemonKill timeout pidFile = fileExist pidFile >>= f
  where
    f False = return ()
    f True = do
      pidRead pidFile >>= g
      removeLink pidFile
    g Nothing = return ()
    g (Just pid) = pidLive pid >>= h pid
    h _   False = return ()
    h pid True = do
      signalProcess sigTERM pid
      wait timeout pid

daemonRunning :: FilePath -> IO Bool
daemonRunning pidFile = fileExist pidFile >>= f
  where
    f False = return False
    f True = pidRead pidFile >>= g
    g Nothing = return False
    g (Just pid) = pidLive pid

daemonize :: FilePath -> IO () -> IO ()
daemonize pidFile program = do
  void $ forkProcess $ do
    void $ createSession
    void $ forkProcess $ do
      pidWrite pidFile
      redirectStdout $ Just "/dev/null"
      redirectStderr $ Just "/dev/null"
      redirectStdin $ Just "/dev/null"
      closeFd stdInput -- close STDIN
      void $ installHandler sigHUP Ignore Nothing
      program
    exitImmediately ExitSuccess
  exitImmediately ExitSuccess

{- Internal -}

-- Wait for a process to exit
-- if it is still running after @secs@
-- seconds, "shoot it in the head"
wait :: Int -> CPid -> IO ()
wait secs pid = (when <$> pidLive pid) >>= \w -> w f
  where f | secs > 0 = do
            usleep 1000000 -- sleep for 1 second
            wait (secs-1) pid
          | otherwise = do
            putStrLn $ "force killing PID " ++ (show pid)
            signalProcess sigKILL pid

pidWrite :: FilePath -> IO ()
pidWrite pidPath = getProcessID >>= writeFile pidPath . show

pidRead :: FilePath -> IO (Maybe CPid)
pidRead pidFile = fileExist pidFile >>= f where
  f True  = fmap (Just . read) . readFile $ pidFile
  f False = return Nothing

pidLive :: CPid -> IO Bool
pidLive pid = (getProcessPriority pid >> return True) `catch` f where
  f :: IOException -> IO Bool
  f _ = return False