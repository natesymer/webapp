{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Web.App.Gzip
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
import Network.Wai.Header
import Network.Wai.Internal
import Data.Maybe (fromMaybe,isJust)
import qualified Codec.Compression.GZip as GZIP (compress)
import qualified Data.ByteString.Char8 as B (ByteString,isInfixOf,break,drop,dropWhile)
import Blaze.ByteString.Builder (toLazyByteString)
import Blaze.ByteString.Builder.ByteString (fromLazyByteString)

-- | Creates a 'Middleware' that GZIPs HTTP responses
gzip :: Integer -- ^ Minimum response length that's GZIP'd
     -> Middleware
gzip minLen app env sendResponse = app env f
  where
    f res@ResponseRaw{} = sendResponse res
    f res
      | isCompressable res = compressResponse res sendResponse
      | otherwise = sendResponse res
    isCompressable res = isAccepted && not isMSIE6 && (not $ isEncoded res) && isBigEnough res
    isAccepted = fromMaybe False . fmap acceptsGZIP . lookup "Accept-Encoding" . requestHeaders $ env
    isMSIE6 = fromMaybe False . fmap (B.isInfixOf "MSIE 6") . lookup "User-Agent" . requestHeaders $ env
    isEncoded = isJust . lookup "Content-Encoding" . responseHeaders
    isBigEnough = maybe True ((<=) minLen) . contentLength . responseHeaders

-- TODO: ensure original flushing action is eval'd
compressResponse :: Response -> (Response -> IO a) -> IO a
compressResponse res sendResponse = f $ lookup "Content-Type" hs
  where
    (s,hs,wb) = responseToStream res
    hs' = (++) [("Vary","Accept-Encoding"),("Content-Encoding","gzip")] . filter ((/=) "Content-Length" . fst) $ hs
    f (Just _) = wb $ \b -> sendResponse $ responseStream s hs' $ \w fl -> b (writeCompressed w) fl
    f _ = sendResponse res
    writeCompressed w = w . fromLazyByteString .  GZIP.compress . toLazyByteString

acceptsGZIP :: B.ByteString -> Bool
acceptsGZIP "" = False
acceptsGZIP x = if y == "gzip"
  then True
  else acceptsGZIP $ skipSpace z
  where (y,z) = B.break (== ',') x
        skipSpace = B.dropWhile (== ' ') . B.drop 1