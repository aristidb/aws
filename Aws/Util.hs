{-# LANGUAGE OverloadedStrings #-}

module Aws.Util
where
  
import           Control.Arrow
import           Control.Exception
import           Data.Time
import           System.Locale
import qualified Data.Ascii           as A
import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Enumerator      as En

tryError :: (Exception e, Monad m) => En.Iteratee a m b -> En.Iteratee a m (Either e b)
tryError m = En.catchError (fmap Right m) h
    where h e = case fromException e of
                  Just v -> return $ Left v
                  Nothing -> En.throwError e

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

fmtTime :: String -> UTCTime -> A.Ascii
fmtTime s t = A.unsafeFromString $ formatTime defaultTimeLocale s t

fmtRfc822Time :: UTCTime -> A.Ascii
fmtRfc822Time = fmtTime "%a, %_d %b %Y %H:%M:%S GMT"

fmtAmzTime :: UTCTime -> A.Ascii
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"

fmtTimeEpochSeconds :: UTCTime -> A.Ascii
fmtTimeEpochSeconds = fmtTime "%s"
