{-# LANGUAGE OverloadedStrings #-}

module Aws.Util
where
  
import           Control.Arrow
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Time
import           System.Locale
import qualified Data.ByteString       as B
import qualified Data.ByteString.UTF8  as BU

queryList :: (a -> [(B.ByteString, B.ByteString)]) -> B.ByteString -> [a] -> [(B.ByteString, B.ByteString)]
queryList f prefix xs = concat $ zipWith combine prefixList (map f xs)
    where prefixList = map (dot prefix . BU.fromString . show) [(1 :: Int) ..]
          combine pf = map $ first (pf `dot`)
          dot x y = B.concat [x, BU.fromString ".", y]

awsBool :: Bool -> B.ByteString
awsBool True = "true"
awsBool False = "false"

awsTrue :: B.ByteString
awsTrue = awsBool True

awsFalse :: B.ByteString
awsFalse = awsBool False

fmtTime :: String -> UTCTime -> B.ByteString
fmtTime s t = BU.fromString $ formatTime defaultTimeLocale s t

fmtRfc822Time :: UTCTime -> B.ByteString
fmtRfc822Time = fmtTime "%a, %_d %b %Y %H:%M:%S GMT"

fmtAmzTime :: UTCTime -> B.ByteString
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"

fmtTimeEpochSeconds :: UTCTime -> B.ByteString
fmtTimeEpochSeconds = fmtTime "%s"
