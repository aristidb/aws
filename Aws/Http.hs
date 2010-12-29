{-# LANGUAGE OverloadedStrings #-}

module Aws.Http
where
  
import qualified Data.ByteString as B

data Protocol
    = HTTP
    | HTTPS
    deriving (Show)

defaultPort :: Protocol -> Int
defaultPort HTTP = 80
defaultPort HTTPS = 443

data Method
    = Get
    | PostQuery
    deriving (Show, Eq)

httpMethod :: Method -> B.ByteString
httpMethod Get = "GET"
httpMethod PostQuery = "POST"
