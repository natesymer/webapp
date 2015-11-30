{-# LANGUAGE OverloadedStrings, TupleSections #-}

module System.WebApp.Cookie 
(
  Cookie(..),
  parseCookie
)
where

import Control.Applicative
import Data.Default
import Data.Char
import Data.Time.Format
import Data.Time.Clock (UTCTime)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 as A

data Cookie = Cookie {
  cookiePairs :: HashMap String String,
  cookiePath :: Maybe String,
  cookieDomain :: Maybe String,
  cookieExpires :: Maybe UTCTime,
  cookieSecure :: Bool,
  cookieHttpOnly :: Bool
} deriving (Show)

instance Default Cookie where
  def = Cookie H.empty Nothing Nothing Nothing False False

-- RFC 1123 date parser
parseCookieDate :: String -> Maybe UTCTime
parseCookieDate = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

-- @flip feed ""@ is required because @pairs@ can
-- take an indefinite amount of input
parseCookie :: B.ByteString -> Maybe Cookie
parseCookie = fmap (lx def) . maybeResult . flip feed "" . parse pairs
  where
    -- lexer
    lx c [] = c
    lx c (("secure",_):xs) = lx (c { cookieSecure = True }) xs
    lx c (("httponly",_):xs) = lx (c { cookieHttpOnly = True }) xs
    lx c (("path",v):xs) = lx (c { cookiePath = Just v }) xs
    lx c (("domain",v):xs) = lx (c { cookieDomain = Just v }) xs
    lx c (("expires",v):xs) = lx (c { cookieExpires = parseCookieDate v }) xs
    lx c ((k,v):xs) = lx (c { cookiePairs = H.insert k v $ cookiePairs c }) xs
    -- grammar
    pairs = (:) <$> pair <*> (many' $ ";" *> skipSpace *> pair)
    pair = (,) <$> attr <*> val
    attr = map toLower <$> many' letter_ascii
    val = option "" $ "=" *> q *> many' letter_ascii <* q
    q = "\"" <|> "'" <|> pure "" -- RFC 6265 states that cookies' values can be quoted