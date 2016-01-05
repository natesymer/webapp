{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Web.App.Streaming
(
  Stream(..),
  stream,
)
where
  
import Network.Wai (StreamingBody)
import Blaze.ByteString.Builder

newtype Stream = Stream { runStream :: StreamingBody }
  
instance Monoid Stream where
  mempty = Stream $ \_ _ -> return ()
  mappend (Stream a) (Stream b) = \w f -> a w f >> b w f

stream :: Builder -> Stream
stream b = Stream $ \w f -> w b >> f
