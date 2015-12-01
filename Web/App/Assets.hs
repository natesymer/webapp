{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.App.Assets
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

General web app operations related to managing assets.
-}

module Web.App.Assets
(
  loadAsset
)
where
  
import Web.App.Monad
import qualified Web.App.FileCache as FC

import Web.Scotty.Trans as Scotty

import Network.HTTP.Types.Status (Status(..))
import Network.Mime

import System.FilePath
import System.Directory

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TL

import qualified Text.CSS.Parse as CSS (parseNestedBlocks)
import qualified Text.CSS.Render as CSS (renderNestedBlocks)
import qualified Text.Jasmine as JS (minifym)

-- | Loads an asset from the 'FileCache' associated with the 'WebAppM' monad.
loadAsset :: (ScottyError e, WebAppState s) => FilePath -- ^ 'FilePath' relative to @assets/@ to load
                                            -> ActionT e (WebAppM s) ()
loadAsset assetsPath = do
  cache <- getCache
  exists <- liftIO $ doesFileExist relPath
  if not exists
    then doesntExist $ B.pack relPath
    else loadFromCache cache
  where
    mimetype = TL.fromStrict . T.decodeUtf8 . defaultMimeLookup . T.pack . takeFileName $ assetsPath
    relPath = "assets/" ++ assetsPath
    doesntExist pth = status . Status 404 $ mconcat ["File ", pth, " does not exist."]
    loadFromCache cache = (liftIO $ FC.lookup cache assetsPath) >>= (f cache)
    f _     (Just (Left err)) = status . Status 500 $ B.pack err
    f _     (Just (Right (cached, md5))) = do
      setHeader "Content-Type" $ mconcat [mimetype, "; charset=utf-8"]
      Scotty.addHeader "Vary" "Accept-Encoding"
      Scotty.setHeader "Content-Encoding" "gzip" -- files in FileCaches are gzipped
      Scotty.header "If-None-Match" >>= h . maybe False (== md5')
      where
        md5' = TL.decodeUtf8 $ BL.fromStrict md5
        h True = Scotty.status $ Status 304 ""
        h False = do
          Scotty.setHeader "ETag" md5'
          Scotty.raw $ BL.fromStrict cached
    f cache Nothing = do
      void $ liftIO $ FC.register' cache assetsPath (g mimetype)
      loadFromCache cache
    builderToBS = BL.toStrict . TL.encodeUtf8 . TL.toLazyText
    g "application/javascript" = fmap BL.toStrict . JS.minifym . BL.fromStrict
    g "text/css" = fmap (builderToBS . CSS.renderNestedBlocks) . CSS.parseNestedBlocks . T.decodeUtf8
    g _ = Right