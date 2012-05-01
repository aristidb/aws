{-# LANGUAGE OverloadedStrings #-}
module Aws.Sqs.Info where

import           Aws.Core
import           Aws.S3.Core
import           Data.Time
import qualified Data.ByteString as B

data SqsAuthorization 
    = SqsAuthorizationHeader 
    | SqsAuthorizationQuery
    deriving (Show)

data Endpoint
    = Endpoint {
        endpointHost :: B.ByteString
      , endpointDefaultLocationConstraint :: LocationConstraint
      , endpointAllowedLocationConstraints :: [LocationConstraint]
      }
    deriving (Show)

data SqsInfo
    = SqsInfo {
        sqsProtocol :: Protocol
      , sqsEndpoint :: Endpoint
      , sqsPort :: Int
      , sqsUseUri :: Bool
      , sqsDefaultExpiry :: NominalDiffTime
      }
    deriving (Show)

sqsEndpointUsClassic :: Endpoint
sqsEndpointUsClassic 
    = Endpoint { 
        endpointHost = "queue.amazonaws.com"
      , endpointDefaultLocationConstraint = locationUsClassic
      , endpointAllowedLocationConstraints = [locationUsClassic
                                             , locationUsWest
                                             , locationEu
                                             , locationApSouthEast
                                             , locationApNorthEast]
      }

sqsEndpointUsWest :: Endpoint
sqsEndpointUsWest
    = Endpoint {
        endpointHost = "us-west-1.queue.amazonaws.com"
      , endpointDefaultLocationConstraint = locationUsWest
      , endpointAllowedLocationConstraints = [locationUsWest]
      }

sqsEndpointEu :: Endpoint
sqsEndpointEu
    = Endpoint {
        endpointHost = "eu-west-1.queue.amazonaws.com"
      , endpointDefaultLocationConstraint = locationEu
      , endpointAllowedLocationConstraints = [locationEu]
      }

sqsEndpointApSouthEast :: Endpoint
sqsEndpointApSouthEast
    = Endpoint {
        endpointHost = "ap-southeast-1.queue.amazonaws.com"
      , endpointDefaultLocationConstraint = locationApSouthEast
      , endpointAllowedLocationConstraints = [locationApSouthEast]
      }

sqsEndpointApNorthEast :: Endpoint
sqsEndpointApNorthEast
    = Endpoint {
        endpointHost = "sqs.ap-northeast-1.amazonaws.com"
      , endpointDefaultLocationConstraint = locationApNorthEast
      , endpointAllowedLocationConstraints = [locationApNorthEast]
      }

sqs :: Protocol -> Endpoint -> Bool -> SqsInfo
sqs protocol endpoint uri 
    = SqsInfo { 
        sqsProtocol = protocol
      , sqsEndpoint = endpoint
      , sqsPort = defaultPort protocol
      , sqsUseUri = uri
      , sqsDefaultExpiry = 15*60
      }

