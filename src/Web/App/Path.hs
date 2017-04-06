{-|
Module      : Web.App.Path
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Path matching/parsing functionality
-}

{-# LANGUAGE OverloadedStrings #-}

module Web.App.Path
(
  -- * Types
  Path,
  -- * Path Constructors
  literal,
  captured,
  regex,
  -- * Path Operations
  isRoot,
  pathMatches,
  pathCaptures,
  -- * Path Components Operations
  splitPathComps,
  joinPathComps,
  isValidPathComps
)
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Text.Regex.Posix
import Data.List
import Data.String
import Data.Char (isAlphaNum)
import Data.Maybe (isJust)
import Data.Bool (bool)
import Control.Monad (join)

{- TYPES -}

-- TODO: CapturedPath needs to store indeces of captures as an optimization

-- |Describes a matchable path.
data Path = LiteralPath [Text] -- ^ as-is
          | CapturedPath [Text] -- ^ path with named wildcards starting with a colon
          | RegexPath Regex -- ^ path with a regex

instance IsString Path where
  fromString = join getCtor . splitPathComps . T.pack
    where
      hasCapture = isJust . find (T.isPrefixOf ":")
      getCtor = bool LiteralPath CapturedPath . hasCapture

{- PATH CONSTRUCTORS -}

-- |Construct a literal 'Path'.
literal :: Text -> Path
literal = LiteralPath . splitPathComps

-- |Construct a captured 'Path'.
captured :: Text -> Path
captured = CapturedPath . splitPathComps

-- |Construct a regex 'Path'.
regex :: Text -> Path
regex = RegexPath . makeRegex . T.encodeUtf8

{- PATH OPERATIONS -}

-- | Returns @True@ if given path is the root.
isRoot :: Path -> Bool
isRoot = flip pathMatches (splitPathComps "/")

-- | Returns @True@ if the given 'Path' matches the given path components.
pathMatches :: Path -- ^ path
            -> [Text] -- ^ pathComps
            -> Bool
pathMatches (RegexPath ex) pin = matchTest ex $ T.encodeUtf8 $ joinPathComps $ delete "/" pin
pathMatches (LiteralPath lpin) pin = lpin == delete "/" pin
pathMatches (CapturedPath cpin) pin = pin == sanitizeCapts cpin (delete "/" pin)
  where sanitizeCapts = zipWith (\c p -> bool c p $ (fst <$> T.uncons c) == Just ':')

-- | Returns path captures by comparing @path@ to @pathComps@.
pathCaptures :: Path -- ^ path
             -> [Text] -- ^ pathComps
             -> [(Text, Text)]
pathCaptures (LiteralPath _) _ = []
pathCaptures (RegexPath r) pin = maybe [] (\(_, x, _, xs) -> f (x:xs)) matched
  where
    f = indexedList . map T.decodeUtf8
    indexedList = zipWith (\a b -> (T.pack $ show a, b)) ([0..] :: [Integer])
    
    matched :: Maybe (B.ByteString, B.ByteString, B.ByteString, [B.ByteString])
    matched = matchM r $ T.encodeUtf8 $ joinPathComps pin
pathCaptures (CapturedPath cap) pin = f [] cap pin
  where
    f acc [] [] = acc
    f _ [] _ = []
    f _ _ [] = []
    f acc (c:cs) (p:ps) = g $ T.uncons c
      where g (Just (':', xs)) = f ((xs, p):acc) cs ps
            g _ = bool [] (f acc cs ps) $ p == c

{- PATH COMPONENTS OPERATIONS -}

-- | Split a 'Text' into a path components.
splitPathComps :: Text -> [Text]
splitPathComps = filter (not . T.null) . T.split (== '/') . T.takeWhile (/= '?')

-- | Join path components into a 'Text'ual path.
joinPathComps :: [Text] -> Text
joinPathComps = mconcat . (:) "/" . intersperse "/"

-- | Determine if some path components contain only valid characters.
isValidPathComps :: [Text] -> Bool
isValidPathComps = all $ \v -> not (T.null v) && T.all isPathChar v
  where isPathChar c = isAlphaNum c || (c `elem` ("-.~!$&'()*+,;=:@%" :: String))
