{-# LANGUAGE OverloadedStrings #-}
module Aws.S3.Info
where

import           Aws.Http
import qualified Data.ByteString as B

data S3Info
    = S3Info {
        s3Protocol :: Protocol
      , s3Host :: B.ByteString
      , s3Port :: Int
      }
    deriving (Show)

s3Http = S3Info { s3Protocol = HTTP, s3Host = "s3.amazonaws.com", s3Port = 80 }