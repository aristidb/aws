{-# LANGUAGE OverloadedStrings #-}

module Aws.Util
where
  
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Char
import           Data.List
import           Data.Time
import           Data.Word
import           System.Locale
import qualified Data.ByteString       as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Set              as S

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap
infixr 5 .:

fmtTime :: String -> UTCTime -> B.ByteString
fmtTime = BU.fromString .: formatTime defaultTimeLocale

fmtRfc822Time :: UTCTime -> B.ByteString
fmtRfc822Time = fmtTime "%a, %_d %b %Y %H:%M:%S GMT"

fmtAmzTime :: UTCTime -> B.ByteString
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"

urlEncodeBS :: B.ByteString -> B.ByteString
urlEncodeBS = B.concatMap (B.pack . encodeChar)
    where
      encodeChar :: Word8 -> [Word8]
      encodeChar ch | ch `S.member` unreserved = [ch]
                    | otherwise                = h2 ch
      
      unreserved :: S.Set Word8
      unreserved = S.fromList . map ord' $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_.~"
      
      h2 :: Word8 -> [Word8]
      h2 v = let (a, b) = v `divMod` 16 in [ord' '%', h a, h b]
      
      h :: Word8 -> Word8
      h i | i < 10    = ord' '0' + i
          | otherwise = ord' 'A' + i - 10

      ord' :: Char -> Word8
      ord' = fromIntegral . ord

urlEncodeVarsBS' :: Bool -> Maybe B.ByteString -> [(B.ByteString, B.ByteString)] -> B.ByteString
urlEncodeVarsBS' useQuestionMark subresource queries = B.concat 
                                                       . addQuestionMark
                                                       . intercalate ["&"] 
                                                       . addSubresource subresource 
                                                       . map showQueryItem 
                                                       $ queries
    where
      addQuestionMark :: [B.ByteString] -> [B.ByteString]
      addQuestionMark [] = []
      addQuestionMark xs | useQuestionMark = "?" : xs
                         | otherwise       = xs
      
      addSubresource :: Maybe B.ByteString -> [[B.ByteString]] -> [[B.ByteString]]
      addSubresource Nothing = id
      addSubresource (Just s) = ([s] :)
      
      showQueryItem :: (B.ByteString, B.ByteString) -> [B.ByteString]
      showQueryItem (n,v) = [urlEncodeBS n, "=", urlEncodeBS v]

urlEncodeVarsBS :: Bool -> [(B.ByteString, B.ByteString)] -> B.ByteString
urlEncodeVarsBS useQuestionMark queries = urlEncodeVarsBS' useQuestionMark Nothing queries
