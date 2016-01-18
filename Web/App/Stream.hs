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
  flush
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

-- |An HTTP response body stream.
newtype Stream = Stream { runStream :: StreamingBody }

instance Monoid Stream where
  mempty = Stream $ \_ _ -> return ()
  mappend (Stream a) (Stream b) = Stream $ \w f -> a w f >> b w f

-- |Flush a stream
flush :: Stream -> Stream
flush (Stream s) = Stream $ \w f -> s w f >> f

-- |Turn data into a WAI stream.
class ToStream a where
  stream :: a -> Stream

instance (ToStream a) => ToStream [a] where
  stream = mconcat . map stream
  
instance ToStream () where
  stream _ = mempty
  
instance ToStream Builder where
  stream b = Stream $ \w f -> w b >> f

instance ToStream Char where
  stream = stream . fromChar

instance ToStream T.Text where
  stream = stream . T.encodeUtf8

instance ToStream TL.Text where
  stream = stream . TL.encodeUtf8
  
instance ToStream B.ByteString where
  stream = stream . fromByteString
  
instance ToStream BL.ByteString where
  stream = stream . fromLazyByteString