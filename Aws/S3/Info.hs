{-# LANGUAGE OverloadedStrings #-}
module Aws.S3.Info
where

import           Aws.Http
import           Aws.S3.Model
import           Data.Time
import qualified Data.ByteString as B

data S3Authorization 
    = S3AuthorizationHeader 
    | S3AuthorizationQuery
    deriving (Show)

data RequestStyle
    = PathStyle
    | BucketStyle
    | VHostStyle
    deriving (Show)

data S3Info
    = S3Info {
        s3Protocol :: Protocol
      , s3Endpoint :: B.ByteString
      , s3RequestStyle :: RequestStyle
      , s3Port :: Int
      , s3UseUri :: Bool
      , s3DefaultExpiry :: NominalDiffTime
      }
    deriving (Show)

s3EndpointUsClassic :: B.ByteString
s3EndpointUsClassic = "s3.amazonaws.com"

s3EndpointUsWest :: B.ByteString
s3EndpointUsWest = "s3-us-west-1.amazonaws.com"

s3EndpointEu :: B.ByteString
s3EndpointEu = "s3-eu-west-1.amazonaws.com"

s3EndpointApSouthEast :: B.ByteString
s3EndpointApSouthEast = "s3-ap-southeast-1.amazonaws.com"

s3EndpointApNorthEast :: B.ByteString
s3EndpointApNorthEast = "s3-ap-northeast-1.amazonaws.com"

s3 :: Protocol -> B.ByteString -> Bool -> S3Info
s3 protocol endpoint uri 
    = S3Info { 
         s3Protocol = protocol
       , s3Endpoint = endpoint
       , s3RequestStyle = BucketStyle
       , s3Port = defaultPort protocol
       , s3UseUri = uri
       , s3DefaultExpiry = 15*60
       }
