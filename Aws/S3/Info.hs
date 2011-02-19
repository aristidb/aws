{-# LANGUAGE OverloadedStrings #-}
module Aws.S3.Info
where

import           Aws.Http
import           Data.Time
import qualified Data.ByteString as B

data S3Authorization 
    = S3AuthorizationHeader 
    | S3AuthorizationQuery
    deriving (Show)                 

data S3Info
    = S3Info {
        s3Protocol :: Protocol
      , s3Host :: B.ByteString
      , s3Port :: Int
      , s3UseUri :: Bool
      , s3DefaultExpiry :: NominalDiffTime
      }
    deriving (Show)

s3Http uri = S3Info { s3Protocol = HTTP, s3Host = "s3.amazonaws.com", s3Port = 80, s3UseUri = uri, s3DefaultExpiry = 15*60 }