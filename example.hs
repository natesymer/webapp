{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
    
import Web.App
import Options.Applicative
import Network.Wai
import Network.HTTP.Types (Status(..))
import Network.HTTP.Types.Method
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy.Char8 as BL
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad.IO.Class

instance WebAppState Integer where
  initState = return 0
  destroyState st = do
    putStr "Counted: "
    print st

main :: IO ()
main = webappMainIO app "My Web App" (Just parseUtil) handleUtil

app :: WebAppT Integer IO ()
app = do
  get (literal "/") $ do
    addHeader "Content-Type" "text/plain"
    getState >>= writeBody . fromString . show

  get (literal "/add") $ do
    modifyState ((+) 1)
    status $ Status 302 ""
    addHeader "Location" "/"
    
  get (literal "/subtract") $ do
    modifyState ((-) 1)
    status $ Status 302 ""
    addHeader "Location" "/"

  get (literal "/reset") $ do
    setState 0
    status $ Status 302 ""
    addHeader "Location" "/"
    
  get (literal "/message") $ do
    writeBody "writeBody \"This is a subpath,\\n\"\n"
    writeBody "writeBody \"hear it roar!\""
    
  get (literal "/pushedpage") $ do
    push methodGet "/image.png"
    writeBody "<div>"
    writeBody "<img src='/image.png'></img>"
    writeBody "</div>"
    
  get (literal "/image.png") $ do
    addHeader "Content-Type" "image/png"
    (liftIO $ BL.readFile "image.png") >>= writeBody . fromLazyByteString

data Util = Password String
  
parseUtil :: Parser Util
parseUtil = subparser $ (mkcmd "password" "Hash a password" parsePassword)
  where
    parsePassword = Password <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc
    
handleUtil :: Util -> IO ()
handleUtil (Password str) = putStrLn str