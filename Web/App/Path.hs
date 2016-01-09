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
  mkQueryDict,
  pathCaptures,
  pathNamedCaptures
)
where

import Network.HTTP.Types (Query)
import Network.HTTP.Types.URI (parseQuery)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Regex
import Data.Maybe
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
regex = RegexPath . mkRegex . T.unpack
    
-- |Returns @True@ if the given 'Path' matches the given 'PathInfo'
pathMatches :: Path -> PathInfo -> Bool
pathMatches (RegexPath ex) pin = isJust $ matchRegex ex $ T.unpack $ joinPath pin
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
isRoot (RegexPath _) = False -- regexes are *assumed* to not match "/".
isRoot (LiteralPath pin) = null pin
isRoot (CapturedPath pin) = null pin

{- General path operations -}

-- |Splits path into (path,queryString).
splitPath :: Text -- ^ path
          -> (Text,Text)
splitPath pth = (p,if T.null q then q else T.tail q)
  where (p,q) = T.span (/= '?') pth

-- |Split @path@ into a pathInfo list.
mkPathInfo :: Text -- ^ path
           -> PathInfo
mkPathInfo = filter (not . T.null) . T.splitOn "/" . fst . splitPath

-- |Join a PathInfo into a path.
joinPath :: PathInfo -- ^ pathInfo
         -> Text
joinPath = mconcat . (:) "/" . intersperse "/"

-- |Return an assoc list containing key-value pairs
-- as expressed in @queryString@.
mkQueryDict :: Text -- queryString
            -> Query
mkQueryDict pth
  | T.null pth = []
  | otherwise = parseQuery $ T.encodeUtf8 pth

{- Captures & Regex Captures -}
      
-- |Returns path captures by comparing @path@ to @pathInfo@.
pathCaptures :: Path -- ^ path
             -> PathInfo -- ^ pathInfo
             -> [(Text,Text)]
pathCaptures (LiteralPath _) _ = []
pathCaptures (RegexPath r) pin = case matchRegexAll r (T.unpack $ joinPath pin) of
  Just (_,matched,_,groups) -> numberList $ (T.pack matched):(map T.pack groups)
  Nothing -> []
pathCaptures (CapturedPath cap) pin = numberList $ f [] cap pin
  where
    f acc [] [] = acc
    f _   _  [] = []
    f _   [] _  = []
    f acc (c:cs) (p:ps)
      | T.head c == ':' = f (p:acc) cs ps
      | p == c = f acc cs ps
      | otherwise = []
      
numberList :: [Text] -> [(Text,Text)]
numberList = zipWith (\a b -> (T.pack $ show a, b)) ([0..] :: [Integer])

-- |Returns named path captures by comparing @path@ to @pathInfo@.
pathNamedCaptures :: Path -- ^ path
                  -> PathInfo -- ^ pathInfo
                  -> [(Text,Text)]
pathNamedCaptures (LiteralPath _) _ = []
pathNamedCaptures (RegexPath _) _ = [] -- TODO: named captures
pathNamedCaptures (CapturedPath cap) pin = f [] cap pin
  where
    f acc [] [] = acc
    f _   _  [] = []
    f _   [] _  = []
    f acc (c:cs) (p:ps)
      | T.head c == ':' = f ((T.tail c,p):acc) cs ps
      | p == c = f acc cs ps
      | otherwise = []

