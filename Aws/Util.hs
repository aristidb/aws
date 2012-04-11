{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Aws.Util where

import Control.Monad.Trans.Control
import           Control.Arrow
import           Control.Exception
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Char
import           Data.Time
import           Data.Word
import           System.Locale
import qualified Control.Exception.Lifted
import qualified Data.ByteString       as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Conduit          as C

tryError :: (Exception e, C.MonadResource m, MonadBaseControl IO m) => m b -> m (Either e b)
tryError = Control.Exception.Lifted.try

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

readHex2 :: [Char] -> Maybe Word8
readHex2 [c1,c2] = do n1 <- readHex1 c1
                      n2 <- readHex1 c2
                      return . fromIntegral $ n1 * 16 + n2
    where
      readHex1 c | c >= '0' && c <= '9' = Just $ ord c - ord '0'
                 | c >= 'A' && c <= 'F' = Just $ ord c - ord 'A' + 10
                 | c >= 'a' && c <= 'f' = Just $ ord c - ord 'a' + 10
      readHex1 _                        = Nothing
readHex2 _ = Nothing
