{-# LANGUAGE OverloadedStrings #-}
module Aws.Route53.Info
where

import           Aws.Http
import           Data.Time
import qualified Data.ByteString as B

--data Route53Authorization = Route53AuthorizationHeader 
--                            deriving (Show)

data Route53Info = Route53Info 
    { route53Protocol :: Protocol
    , route53Endpoint :: B.ByteString
    , route53Port :: Int
    } deriving (Show)

route53EndpointUsClassic :: B.ByteString
route53EndpointUsClassic = "route53.amazonaws.com"

route53 :: Route53Info
route53 = Route53Info 
    { route53Protocol = HTTPS
    , route53Endpoint = route53EndpointUsClassic
    , route53Port = defaultPort HTTPS
    }
