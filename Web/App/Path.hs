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
  mkQueryDict,
  pathCaptures,
  regexPathCaptures,
  regexPathNamedCaptures
)
where

import Network.HTTP.Types (Query)
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
      
literal :: Text -> Path
literal = LiteralPath . mkPathInfo

captured :: Text -> Path
captured = CapturedPath . mkPathInfo

regex :: Text -> Path
regex = RegexPath . mkRegex . T.unpack
    
-- |Returns @True@ if the given 'Path' matches the given 'PathInfo'
pathMatches :: Path -> PathInfo -> Bool
pathMatches (RegexPath ex) pin = isJust $ matchRegex ex path
  where path = T.unpack $ mconcat $ "/":(intersperse "/" pin)
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
           -> [Text]
mkPathInfo = filter (not . T.null) . T.splitOn "/" . fst . splitPath

-- |Return an assoc list containing key-value pairs
-- as expressed in @queryString@.
mkQueryDict :: Text -- queryString
            -> Query
mkQueryDict pth
  | T.null pth = []
  | otherwise = map splitPair $ T.splitOn "&" pth
    where
      splitPair p 
        | T.null ev = (T.encodeUtf8 k, Nothing)
        | T.length ev == 1 = (T.encodeUtf8 k, Nothing)
        | otherwise = (T.encodeUtf8 k, Just $ T.encodeUtf8 $ T.tail ev)
        where (k,ev) = T.breakOn "=" p

{- Captures & Regex Captures -}

-- |Returns the captures by comparing @path@ to @capturedPath@.
pathCaptures :: [Text] -- ^ capturedPath
             -> Text -- ^ path
             -> [(Text,Text)]
pathCaptures cap pth = f [] cap $ mkPathInfo pth
  where
    f acc [] [] = acc
    f _   _  [] = []
    f _   [] _  = []
    f acc (c:cs) (p:ps)
      | T.head c == ':' = f ((T.tail c,p):acc) cs ps
      | p == c = f acc cs ps
      | otherwise = []
    
-- TODO: implement functions below
               
-- |Returns matched unnamed capture groups in @regex@
-- by matching @regex@ with path.
regexPathCaptures :: Text -- ^ regex
                  -> Text -- ^ path
                  -> [Text]
regexPathCaptures _ _ = []
                  
               
-- |Returns matched named capture groups in @regex@
-- by matching @regex@ with path.
regexPathNamedCaptures :: Text -- ^ regex
                       -> Text -- ^ path
                       -> [(Text,Text)]
regexPathNamedCaptures _ _ = []