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

data Endpoint
    = Endpoint {
        endpointHost :: B.ByteString
      , endpointDefaultLocationConstraint :: LocationConstraint
      , endpointAllowedLocationConstraints :: [LocationConstraint]
      }
    deriving (Show)

data S3Info
    = S3Info {
        s3Protocol :: Protocol
      , s3Endpoint :: Endpoint
      , s3Port :: Int
      , s3UseUri :: Bool
      , s3DefaultExpiry :: NominalDiffTime
      }
    deriving (Show)

s3EndpointUsClassic 
    = Endpoint { 
        endpointHost = "s3.amazonaws.com"
      , endpointDefaultLocationConstraint = locationUsClassic
      , endpointAllowedLocationConstraints = [locationUsClassic
                                             , locationUsWest
                                             , locationEu
                                             , locationApSouthEast
                                             , locationApNorthEast]
      }

s3EndpointUsWest
    = Endpoint {
        endpointHost = "s3-us-west-1.amazonaws.com"
      , endpointDefaultLocationConstraint = locationUsWest
      , endpointAllowedLocationConstraints = [locationUsWest]
      }

s3EndpointEu
    = Endpoint {
        endpointHost = "s3-eu-west-1.amazonaws.com"
      , endpointDefaultLocationConstraint = locationEu
      , endpointAllowedLocationConstraints = [locationEu]
      }

s3EndpointApSouthEast
    = Endpoint {
        endpointHost = "s3-ap-southeast-1.amazonaws.com"
      , endpointDefaultLocationConstraint = locationApSouthEast
      , endpointAllowedLocationConstraints = [locationApSouthEast]
      }

s3EndpointApNorthEast
    = Endpoint {
        endpointHost = "s3-ap-northeast-1.amazonaws.com"
      , endpointDefaultLocationConstraint = locationApNorthEast
      , endpointAllowedLocationConstraints = [locationApNorthEast]
      }

s3 :: Protocol -> Endpoint -> Bool -> S3Info
s3 protocol endpoint uri 
    = S3Info { 
        s3Protocol = protocol
      , s3Endpoint = s3EndpointUsClassic
      , s3Port = defaultPort protocol
      , s3UseUri = uri
      , s3DefaultExpiry = 15*60
      }
