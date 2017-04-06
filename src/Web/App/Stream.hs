{-|
Module      : Web.App.Stream
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

@newtype@ wrapper around a WAI 'StreamingBody'.
-}

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Web.App.Stream
(
  Stream(..),
  ToStream(..),
  flush,
  flusher
)
where
  
import Network.Wai (StreamingBody)
import Blaze.ByteString.Builder hiding (flush)
import Blaze.ByteString.Builder.Char8 hiding (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Monoid
import Data.Bool

-- |An HTTP response body stream.
newtype Stream = Stream { runStream :: StreamingBody }

instance Monoid Stream where
  mempty = Stream $ \_ _ -> return ()
  mappend (Stream a) (Stream b) = Stream $ \w f -> a w f >> b w f

-- |Flush a stream.
flush :: Stream -> Stream
flush s = s <> flusher

-- |A stream that flushes written data.
flusher :: Stream
flusher = Stream $ \_ f -> f

-- |Turn data into a WAI stream.
class ToStream a where
  stream :: Bool -> a -> Stream -- ^ stream with the option to flush
  stream' :: a -> Stream -- ^ stream and flush
  stream' = stream True

instance (ToStream a) => ToStream [a] where
  stream True b = (stream False b) <> flusher
  stream False b = mconcat $ map (stream False) b
  
instance ToStream Stream where
  stream = bool id flush
  
instance ToStream () where
  stream _ _ = mempty
  
instance ToStream Builder where
  stream False b = Stream $ \w _ -> w b
  stream True b = Stream $ \w f -> w b >> f

instance ToStream Char where
  stream f = stream f . fromChar

instance ToStream T.Text where
  stream f = stream f . T.encodeUtf8

instance ToStream TL.Text where
  stream f = stream f . TL.encodeUtf8
  
instance ToStream B.ByteString where
  stream f = stream f . fromByteString
  
instance ToStream BL.ByteString where
  stream f = stream f . fromLazyByteString