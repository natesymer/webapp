{-# LANGUAGE OverloadedStrings #-}
module Web.App.RouteMatching
(
  -- * Route predicates
  literal,
  captured,
  regex,
  -- * HTTP matching actions
  get,
  post,
  put,
  patch,
  delete,
  options,
  anyVerb,
  anyRequest
)
where
  
import Web.App.Monad.WebAppT
import Web.App.State
import Web.App.Monad.RouteT
  
import Network.Wai (Request(..))
import Network.HTTP.Types.Method

import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex
import qualified Data.ByteString.Char8 as B
import Data.Maybe

literal :: Text -> Request -> Bool
literal path req
  | T.head path == '/' = (T.intercalate "/" (pathInfo req)) == (T.tail path)
  | otherwise = False

captured :: Text -> Request -> Bool
captured path req
  | T.head path == '/' = f (T.tail path) $ pathInfo req
  | otherwise = False
  where
    f p [] = T.null p
    f p (x:xs) = let (a,b) = T.splitAt (T.length x) p
                 in a == x && f (T.drop 1 b) xs

regex :: Text -> Request -> Bool
regex ex req = isJust $ matchRegex (mkRegex $ T.unpack ex) (B.unpack $ rawPathInfo req)

get :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
get p act = route (\r -> ((requestMethod r) == methodGet) && (p r)) act

post :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
post p act = route (\r -> ((requestMethod r) == methodPost) && (p r)) act

put :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
put p act = route (\r -> ((requestMethod r) == methodPut) && (p r)) act

patch :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
patch p act = route (\r -> ((requestMethod r) == methodPatch) && (p r)) act

delete :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
delete p act = route (\r -> ((requestMethod r) == methodDelete) && (p r)) act

options :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
options p act = route (\r -> ((requestMethod r) == methodOptions) && (p r)) act

anyVerb :: (WebAppState s, Monad m) => Predicate -> RouteT s m () -> WebAppT s m ()
anyVerb = route

anyRequest :: (WebAppState s, Monad m) => RouteT s m () -> WebAppT s m ()
anyRequest = route (const True)