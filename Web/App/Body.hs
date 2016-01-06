module Web.App.Body
(
  bodyByteString
)
where

import Network.Wai

import System.IO.Unsafe

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.Internal (Chunk,Empty)

bodyByteString :: Request -> IO BL.ByteString
bodyByteString r = lazyRead
  where
    lazyRead = unsafeInterleaveIO $ do
      c <- requestBody r
      if B.null c
        then return Empty
        else Chunk c <$> lazyRead