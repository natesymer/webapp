{-# LANGUAGE OverloadedStrings #-}
module Web.App.Password
(
  hashPassword
)
where

import Crypto.BCrypt hiding (hashPassword)
import qualified Data.ByteString.Char8 as B (pack,unpack)

hashPassword :: String -> IO (Maybe String)
hashPassword pwd = do
  hsh <- hashPasswordUsingPolicy (HashingPolicy 12 "$2b$") (B.pack pwd)
  return $ fmap B.unpack hsh
