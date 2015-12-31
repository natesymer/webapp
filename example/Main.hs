{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
    
import Web.App
import Web.Scotty.Trans
import Options.Applicative
import Network.HTTP.Types (Status(..))
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.IO.Class

instance WebAppState Integer where
  initState = return 0
  destroyState st = do
    putStr "Counted: "
    print st

main :: IO ()
main = webappMain app "My Web App" (Just parseUtil) handleUtil

app :: WebAppT Integer IO ()
app = do
  route (\r -> True) $ do
    writeBody "Touch my body!"

-- app :: ScottyT Text (WebAppM Integer) ()
-- app = do
--   get "/" $ do
--     getState >>= raw . BL.pack . show
--
--   get "/add" $ do
--     modifyState ((+) 1)
--     status $ Status 302 ""
--     setHeader "Location" "/"
--
--   get "/subtract" $ do
--     count <- getState
--     putState $ count-1
--     status $ Status 302 ""
--     setHeader "Location" "/"
--
--   get "/assets/:file" $ param "file" >>= loadAsset
  
data Util = Password String
  
parseUtil :: Parser Util
parseUtil = subparser $ (mkcmd "password" "Hash a password" parsePassword)
  where
    parsePassword = Password <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc
    
handleUtil :: Util -> IO ()
handleUtil (Password str) = putStrLn str