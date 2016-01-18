{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}
module Main (main) where
    
import Web.App
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
main = webappMainIO app "My Web App" (Just parseUtil) handleUtil

app :: WebApp Integer IO
app = mconcat [
  -- Counter routes
  get "/" root,
  get "/add" add,
  get "/add/specific" addSpecific,
  get "/subtract" subtr,
  get "/reset" reset,
  -- Extended example routes
  get "/fallthrough" fallthrough,
  get "/message" message,
  get "/captured/:id" withC,
  get (regex "/assets/(.*)") withR,
  matchAll $ writeBodyBytes "not found!\n"]
  where
    root = do
      addHeader "Content-Type" "text/plain"
      S.get >>= writeBody . show
    add = do
      S.state (((),) . (+) 1)
      redirect "/"
    addSpecific = do
      v <- param "v"
      S.state (((),) . (+) v)
      redirect "/"
    subtr = do
      S.state (((),) . (-) 1)
      redirect "/"
    reset = do
      S.put 0
      redirect "/"
    fallthrough = do
      liftIO $ putStrLn "falling through..."
      next
    message = do
      writeBodyBytes "writeBodyBytes \"This is a subpath,\\n\"\n"
      writeBodyBytes "writeBodyBytes \"hear it roar!\""
    withC = do
      param "id" >>= \(v :: Double) -> writeBody $ show v
      liftIO $ print "after"
    withR = do
      writeBodyBytes "asset path: "
      param "1" >>= writeBodyBytes
      writeBody '\n'

data Util = Password String
  
parseUtil :: Parser Util
parseUtil = subparser $ (mkcmd "password" "Hash a password" parsePassword)
  where
    parsePassword = Password <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc
    
handleUtil :: Util -> IO ()
handleUtil (Password str) = putStrLn str