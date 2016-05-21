{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Web.App.Middleware.Gzip
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

WAI middleware to GZIP HTTP responses.
-}

module Web.App.Middleware.Gzip
(
  gzip
)
where

import Network.Wai
import Network.Wai.Internal
import Data.Maybe (fromMaybe)
import qualified Codec.Compression.GZip as GZIP (compress)
import qualified Data.ByteString.Char8 as B (readInt,isInfixOf)
import Blaze.ByteString.Builder (toLazyByteString)
import Blaze.ByteString.Builder.ByteString (fromLazyByteString)

-- | Creates a 'Middleware' that GZIPs HTTP responses
gzip :: Int -- ^ Minimum response length that's GZIP'd
     -> Middleware
gzip minLen app env sendResponse = app env f
  where
    f res@ResponseRaw{} = sendResponse res
    f res
      | isCompressable = sendCompressed res sendResponse
      | otherwise = sendResponse res
      where
        isCompressable = isAccepted && not isIE6 && isBigEnough && not isEncoded
        isAccepted = headerContains "gzip" "Accept-Encoding" $ requestHeaders env
        isIE6 = headerContains "MSIE 6" "User-Agent" $ requestHeaders env
        isEncoded = headerContains "gzip" "Content-Encoding" $ responseHeaders res
        isBigEnough = maybe True ((<=) minLen) contentLength
        contentLength = getResHeader "Content-Length" >>= fmap fst . B.readInt
        getResHeader k = lookup k $ responseHeaders res
        headerContains s k hdrs = fromMaybe False $ B.isInfixOf s <$> lookup k hdrs

sendCompressed :: Response -> (Response -> IO a) -> IO a
sendCompressed res f = maybe (f res) (const send) $ lookup "Content-Type" hs
  where
    (s,hs,wb) = responseToStream res
    invariants = [("Vary","Accept-Encoding"),("Content-Encoding","gzip")]
    hs' = invariants ++ (filter ((/=) "Content-Length" . fst) hs)
    send = wb $ \b -> f $ responseStream s hs' (b . (. compress))
    compress = fromLazyByteString . GZIP.compress . toLazyByteString