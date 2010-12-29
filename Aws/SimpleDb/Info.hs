{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Aws.SimpleDb.Info
where

import           Aws.Http
import           Aws.Query
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B

data SdbInfo
    = SdbInfo {
        sdbiProtocol :: Protocol
      , sdbiHttpMethod :: Method
      , sdbiHost :: B.ByteString
      , sdbiPort :: Int
      }
    deriving (Show)
             
sdbUsEast :: B.ByteString
sdbUsEast = "sdb.amazonaws.com" 

sdbUsWest :: B.ByteString
sdbUsWest = "sdb.us-west-1.amazonaws.com"

sdbEuWest :: B.ByteString
sdbEuWest = "sdb.eu-west-1.amazonaws.com"

sdbApSoutheast :: B.ByteString
sdbApSoutheast = "sdb.ap-southeast-1.amazonaws.com"
             
sdbHttpGet :: B.ByteString -> SdbInfo
sdbHttpGet endpoint = SdbInfo HTTP Get endpoint (defaultPort HTTP)
                          
sdbHttpPost :: B.ByteString -> SdbInfo
sdbHttpPost endpoint = SdbInfo HTTP PostQuery endpoint (defaultPort HTTP)
              
sdbHttpsGet :: B.ByteString -> SdbInfo
sdbHttpsGet endpoint = SdbInfo HTTPS Get endpoint (defaultPort HTTPS)
             
sdbHttpsPost :: B.ByteString -> SdbInfo
sdbHttpsPost endpoint = SdbInfo HTTPS PostQuery endpoint (defaultPort HTTPS)

sdbiBaseQuery :: SdbInfo -> Query
sdbiBaseQuery SdbInfo{..} = Query { 
                              api = SimpleDB
                            , method = sdbiHttpMethod
                            , protocol = sdbiProtocol
                            , host = sdbiHost
                            , port = sdbiPort 
                            , path = "/"
                            , canonicalizedResource = ""
                            , query = [("Version", "2009-04-15")]
                            , date = Nothing
                            , contentType = Nothing
                            , contentMd5 = Nothing
                            , body = L.empty
                            }
