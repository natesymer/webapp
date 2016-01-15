{-# LANGUAGE OverloadedStrings #-}

module Web.App.Parameter
(
  Parameter(..)
)
where
  
import Data.Maybe
  
import Data.Word
import Data.Int
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)
import qualified Data.ByteString.Char8 as B (ByteString,unpack,length,head,null,split)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString,fromStrict)
  
class Parameter a where
  maybeRead :: (Read a) => B.ByteString -> Maybe a
  maybeRead s = case [ x | (x,"") <- reads (B.unpack s) ] of
    [x] -> Just x
    _ -> Nothing
  maybeReadList :: (Read a) => B.ByteString -> [a]
  maybeReadList = catMaybes . map maybeRead . B.split ','
    
instance Parameter () where
  maybeRead x
    | B.null x = Just ()
    | otherwise = Nothing
    
instance Parameter Bool where
  maybeRead "True" = Just True
  maybeRead "true" = Just True
  maybeRead "T" = Just True
  maybeRead "t" = Just True
  maybeRead "Yes" = Just True
  maybeRead "yes" = Just True
  maybeRead "Y" = Just True
  maybeRead "y" = Just True
  maybeRead "False" = Just False
  maybeRead "false" = Just False
  maybeRead "F" = Just False
  maybeRead "f" = Just False
  maybeRead "No" = Just False
  maybeRead "no" = Just False
  maybeRead "N" = Just False
  maybeRead "n" = Just False
  maybeRead _ = Nothing

instance Parameter T.Text where
  maybeRead = Just . T.decodeUtf8
  
instance Parameter TL.Text where
  maybeRead = fmap TL.decodeUtf8 . maybeRead

instance Parameter B.ByteString where
  maybeRead = Just
  
instance Parameter BL.ByteString where
  maybeRead = Just . BL.fromStrict
  
instance Parameter Char where
  maybeRead x
    | B.length x == 1 = Just $ B.head x
    | otherwise = Nothing
  maybeReadList = B.unpack
    
instance (Parameter a, Read a) => Parameter [a] where
  maybeRead = Just . maybeReadList
  
instance Parameter Double
instance Parameter Float

instance Parameter Int
instance Parameter Int8
instance Parameter Int16
instance Parameter Int32
instance Parameter Int64
instance Parameter Integer  

instance Parameter Word
instance Parameter Word8
instance Parameter Word16
instance Parameter Word32
instance Parameter Word64