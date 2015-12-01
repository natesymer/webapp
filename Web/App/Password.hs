{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.App.Password
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Generate BCrypt hashes.
-}

module Web.App.Password
(
  -- * Password Hashing
  hashPassword
)
where

import Crypto.BCrypt hiding (hashPassword)
import qualified Data.ByteString.Char8 as B (pack,unpack)

-- | Hash a password with the most strict 'HashingPolicy'
hashPassword :: String -- ^ Password to hash
             -> IO (Maybe String)
hashPassword pwd = do
  hsh <- hashPasswordUsingPolicy (HashingPolicy 12 "$2b$") (B.pack pwd)
  return $ fmap B.unpack hsh
