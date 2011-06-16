{-# LANGUAGE OverloadedStrings #-}
module Aws.SQS.Info where

import           Aws.Http
import qualified Data.ByteString as B

data SqsInfo 
    = SqsInfo{
          sqsiProtocol :: Protocol
        , sqsiHttpMethod :: Method
        , sqsiHost :: B.ByteString
        , sqsiPort :: Int
}

sqsUsEast :: B.ByteString
sqsUsEast = "sqs.us-east-1.amazonaws.com"

sqsUsWest :: B.ByteString
sqsUsWest = "sqs.us-west-1.amazonaws.com"

sqsEuWest :: B.ByteString
sqsEuWest = "sqs.eu-west-1.amazonaws.com"

sqsApSoutheast :: B.ByteString
sqsApSoutheast = "sqs.ap-southeast-1.amazonaws.com"

sqsApNortheast :: B.ByteString
sqsApNortheast = "sqs.ap-northeast-1.amazonaws.com"

sdbHttpGet :: B.ByteString -> SdbInfo
sdbHttpGet endpoint = SqsInfo HTTP Get endpoint (defaultPort HTTP)
                          
sdbHttpPost :: B.ByteString -> SdbInfo
sdbHttpPost endpoint = SqsInfo HTTP PostQuery endpoint (defaultPort HTTP)
              
sdbHttpsGet :: B.ByteString -> SdbInfo
sdbHttpsGet endpoint = SqsInfo HTTPS Get endpoint (defaultPort HTTPS)
             
sdbHttpsPost :: B.ByteString -> SdbInfo
sdbHttpsPost endpoint = SqsInfo HTTPS PostQuery endpoint (defaultPort HTTPS)

