{-|
Module      : Web.App.Path
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

A URI pathInfo wrapper.
-}

{-# LANGUAGE OverloadedStrings #-}

module Web.App.Path
(
  -- * Types
  Path,
  PathInfo,
  -- * Path Constructors
  literal,
  captured,
  regex,
  -- * Path Operations
  pathMatches,
  -- * Text-Based Path Operations
  isRoot,
  splitPath,
  mkPathInfo,
  joinPath,
  pathCaptures
)
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Text.Regex.Posix
import Data.List
import Data.String

-- |Represents a PathInfo (e.g. from WAI)
type PathInfo = [Text]

-- |Describes a matchable path.
data Path = LiteralPath PathInfo -- ^ Match a path as-is
          | CapturedPath PathInfo -- ^ Match a path with wildcards
          | RegexPath Regex -- ^ Match a path with a regex
          
instance IsString Path where
  fromString = f . mkPathInfo . T.pack
    where f pinfo = case find (T.isPrefixOf ":") pinfo of
            Just _ -> CapturedPath pinfo
            Nothing -> LiteralPath pinfo
      
-- |Construct a literal 'Path'.
literal :: Text -> Path
literal = LiteralPath . mkPathInfo

-- |Construct a captured 'Path'.
captured :: Text -> Path
captured = CapturedPath . mkPathInfo

-- |Construct a regex 'Path'.
regex :: Text -> Path
regex = RegexPath . makeRegex . T.encodeUtf8

-- |Returns @True@ if the given 'Path' matches the given 'PathInfo'
pathMatches :: Path -> PathInfo -> Bool
pathMatches (RegexPath ex) pin = matchTest ex $ T.encodeUtf8 $ joinPath pin
pathMatches (LiteralPath pin) pin2 = pin == pin2
pathMatches (CapturedPath pin) pin2 = f pin pin2
  where f [] [] = True
        f _  [] = False
        f [] _  = False
        f (c:cs) (p:ps)
          | T.head c == ':' = f cs ps
          | p == c = f cs ps
          | otherwise = False

-- |Returns true if given path is the root.
isRoot :: Path -> Bool
isRoot (RegexPath ex) = matchTest ex ("/" :: String)
isRoot (LiteralPath pin) = null pin
isRoot (CapturedPath pin) = null pin

{- General path operations -}

-- |Splits path into (path,queryString).
splitPath :: Text -- ^ path
          -> (Text,Text)
splitPath pth = (p,T.drop 1 q)
  where (p,q) = T.break (== '?') pth

-- |Split @path@ into a pathInfo list.
mkPathInfo :: Text -- ^ path
           -> PathInfo
mkPathInfo = filter (not . T.null) . T.splitOn "/" . fst . splitPath

-- |Join pathInfo into a 'Text'ual path.
joinPath :: PathInfo -- ^ pathInfo
         -> Text
joinPath = mconcat . (:) "/" . intersperse "/"

{- Captures & Regex Captures -}
      
-- |Returns path captures by comparing @path@ to @pathInfo@.
pathCaptures :: Path -- ^ path
             -> PathInfo -- ^ pathInfo
             -> [(Text,Text)]
pathCaptures (LiteralPath _) _ = []
pathCaptures (RegexPath r) pin = maybe [] (\(_,x,_,xs) -> f (x:xs)) matched
  where
    f = numberList . map T.decodeUtf8
    matched :: Maybe (B.ByteString,B.ByteString,B.ByteString,[B.ByteString])
    matched = matchM r $ T.encodeUtf8 $ joinPath pin

pathCaptures (CapturedPath cap) pin = f [] cap pin
  where
    f acc [] [] = acc
    f _   _  [] = []
    f _   [] _  = []
    f acc (c:cs) (p:ps)
      | T.head c == ':' = f ((T.tail c,p):acc) cs ps
      | p == c = f acc cs ps
      | otherwise = []
      
numberList :: [Text] -> [(Text,Text)]
numberList = zipWith (\a b -> (T.pack $ show a, b)) ([0..] :: [Integer])