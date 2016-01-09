{-# LANGUAGE OverloadedStrings, TupleSections #-}
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

app :: WebAppT Integer IO ()
app = do
  get "/" $ do
    addHeader "Content-Type" "text/plain"
    S.get >>= writeBody . fromString . show

  get "/add" $ do
    S.state (((),) . (+) 1)
    redirect "/"
    
  get "/subtract" $ do
    S.state (((),) . (-) 1)
    redirect "/"

  get "/reset" $ do
    S.put 0
    redirect "/"
    
  get "/message" $ do
    writeBody "writeBody \"This is a subpath,\\n\"\n"
    writeBody "writeBody \"hear it roar!\""
    
  get "/pushedpage" $ do
    addHeader "Content-Type" "text/html"
    push methodGet "/image.png"
    writeBody "<div>"
    writeBody "<img src='/image.png'></img>"
    writeBody "</div>"
    
  get "/image.png" $ do
    addHeader "Content-Type" "image/png"
    (liftIO $ BL.readFile "image.png") >>= writeBody . fromLazyByteString
    
  post "/echobody" $ do
    body >>= writeBody . fromLazyByteString
    body >>= liftIO . print
    
  get "/fallthrough" $ do
    liftIO $ putStrLn "falling through..."
    next

  get (regex ".*") $ do
    writeBody "fell through!"

data Util = Password String
  
parseUtil :: Parser Util
parseUtil = subparser $ (mkcmd "password" "Hash a password" parsePassword)
  where
    parsePassword = Password <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc
    
handleUtil :: Util -> IO ()
handleUtil (Password str) = putStrLn str