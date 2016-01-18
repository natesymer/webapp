{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Web.App.Parameter
(
  Parameter(..)
)
where
  
import GHC.Float (double2Float)
import Data.Maybe
import Data.Word
import Data.Int
import Data.Char
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)
import qualified Data.ByteString.Char8 as B -- (ByteString,unpack,length,head,null,split,readInt)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString,fromStrict)
  
class Parameter a where
  maybeRead :: B.ByteString -> Maybe a
  maybeReadList :: B.ByteString -> [a]
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
    
instance (Parameter a) => Parameter [a] where
  maybeRead = Just . maybeReadList
  
instance Parameter Double where
  maybeRead "" = Nothing
  maybeRead (B.uncons -> Just ('-',xs)) = negate <$> maybeRead xs
  maybeRead (B.uncons -> Just ('.',v)) = (*) e <$> v''
    where v'  = B.takeWhile isDigit v
          v'' = fromIntegral . fst <$> B.readInt v'
          e   = 10**(negate $ fromIntegral $ B.length v')
  maybeRead x = (+) (fromMaybe 0 $ maybeRead xs) <$> x''
    where (x',xs) = B.span isDigit x
          x''     = fromInteger . fst <$> B.readInteger x'

instance Parameter Float where
  maybeRead = fmap double2Float . maybeRead

instance Parameter Integer where
  maybeRead = fmap fst . B.readInteger

instance Parameter Int where
  maybeRead = fmap fst . B.readInt

instance Parameter Int8 where
  maybeRead = fmap (fromInteger . fst) . B.readInteger
  
instance Parameter Int16 where
  maybeRead = fmap (fromInteger . fst) . B.readInteger
  
instance Parameter Int32 where
  maybeRead = fmap (fromInteger . fst) . B.readInteger
  
instance Parameter Int64 where
  maybeRead = fmap (fromInteger . fst) . B.readInteger

instance Parameter Word where
  maybeRead = fmap (fromInteger . fst) . B.readInteger
  
instance Parameter Word8 where
  maybeRead = fmap (fromInteger . fst) . B.readInteger
  
instance Parameter Word16 where
  maybeRead = fmap (fromInteger . fst) . B.readInteger
  
instance Parameter Word32 where
  maybeRead = fmap (fromInteger . fst) . B.readInteger
  
instance Parameter Word64 where
  maybeRead = fmap (fromInteger . fst) . B.readInteger