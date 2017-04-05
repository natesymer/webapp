{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}
module Main (main) where
    
import Web.App
import Data.Monoid
import Options.Applicative
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Method
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy.Char8 as BL
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad.IO.Class

import qualified Control.Monad.State.Class as S

instance WebAppState Integer where
  initState = return 0
  destroyState st = do
    putStr "Counted: "
    print st

main :: IO ()
main = webappMainIO app [] (Just parseUtil) handleUtil

app :: [Route Integer IO]
app = [
  -- Counter routes
  get "/" root,
  get "/add" add,
  get "/add/specific" addSpecific,
  get "/subtract" subtr,
  get "/reset" reset,
  -- Extended example routes
  get "/fallthrough" fallthrough,
  get "/message" message,
  get "/teststring" streamString,
  get "/captured/:id" withC,
  get (regex "/assets/(.*)") withR,
  matchAll $ writeBody ("not found!\n" :: String)]
  where
    root = do
      addHeader "Content-Type" "text/plain"
      getState >>= writeBody . show
    add = do
      getState >>= putState . (+) 1
      redirect "/"
    addSpecific = do
      v <- param "v"
      getState >>= putState . (+) v
      redirect "/"
    subtr = do
      getState >>= putState . (-) 1
      redirect "/"
    reset = do
      putState 0
      redirect "/"
    fallthrough = do
      liftIO $ putStrLn "falling through..."
      next
    message = do
      writeBody ("writeBody \"This is a subpath,\\n\"\n" :: String)
      writeBody ("writeBody \"hear it roar!\"" :: String)
    streamString = do
      writeBody ("this is a string" :: String)
    withC = do
      param "id" >>= \(v :: Double) -> writeBody $ show v
    withR = do
      writeBody ("asset path: " :: String)
      param "1" >>= \v -> writeBody (v :: String)
      writeBody '\n'

data Util = Password String
  
parseUtil :: Parser Util
parseUtil = subparser $ mkcmd "password" "Hash a password" parsePassword
  where
    parsePassword = Password <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc
    
handleUtil :: Util -> IO ()
handleUtil (Password str) = putStrLn str
