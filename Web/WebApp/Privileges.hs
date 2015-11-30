module System.WebApp.Privileges
(
  
  isPrivileged,
  whenPrivileged,
  resignPrivileges
)
where

import Control.Monad
import System.Posix

isPrivileged :: IO Bool
isPrivileged = ((==) 0) <$> getEffectiveUserID

whenPrivileged :: IO () -> IO ()
whenPrivileged act = do
  privileged <- isPrivileged
  when privileged act
  
resignPrivileges :: String -> IO ()
resignPrivileges user = whenPrivileged $ do
  getUserEntryForName user >>= setUserID . userID