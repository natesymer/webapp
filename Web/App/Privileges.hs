{-|
Module      : Web.App.Privileges
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Determine when the process is being ran as root.
-}

module Web.App.Privileges
(
  isPrivileged,
  whenPrivileged,
  resignPrivileges
)
where

import Control.Monad
import System.Posix

-- | Determine if the process is being ran as root
isPrivileged :: IO Bool
isPrivileged = ((==) 0) <$> getEffectiveUserID

-- | Perform an action when ran as root
whenPrivileged :: IO () -- ^ The action to be performed
               -> IO ()
whenPrivileged act = do
  privileged <- isPrivileged
  when privileged act
  
-- | Drop root privileges
resignPrivileges :: String -- ^ Name of user to "become"
                 -> IO ()
resignPrivileges user = whenPrivileged $ do
  getUserEntryForName user >>= setUserID . userID