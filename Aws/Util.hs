{-# LANGUAGE OverloadedStrings #-}

module Aws.Util
where
  
import           Data.Char
import           Data.List
import           Data.Time
import           System.Locale
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString       as B
import qualified Data.Set              as S

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap
infixr 5 .:

fmtTime :: String -> UTCTime -> String
fmtTime = formatTime defaultTimeLocale

fmtRfc822Time :: UTCTime -> String
fmtRfc822Time = fmtTime "%a, %_d %b %Y %H:%M:%S GMT"

fmtAmzTime :: UTCTime -> String
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"

urlEncodeBS :: B.ByteString -> B.ByteString
urlEncodeBS = B.concatMap (B.pack . map fromIntegral . encodeChar . fromIntegral)
    where
      encodeChar ch | ch `S.member` unreserved = [ch]
                    | otherwise                = h2 ch
      unreserved = S.fromList . map ord $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_.~"
      h2 v = let (a, b) = v `divMod` 16 in [ord '%', h a, h b]
      h i | i < 10    = ord '0' + i
          | otherwise = ord 'A' + i - 10

urlEncodeVarsBS' :: Bool -> Maybe B.ByteString -> [(B.ByteString, B.ByteString)] -> B.ByteString
urlEncodeVarsBS' q subresource queries = B.concat . addQ q . intercalate ["&"] . addS subresource . map showQ $ queries
    where addQ True [] = []
          addQ True xs = "?" : xs
          addQ False xs = xs
          addS Nothing = id
          addS (Just s) = ([s] :)
          showQ (n,v) = [urlEncodeBS n, "=", urlEncodeBS v]
