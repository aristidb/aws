{-# LANGUAGE OverloadedStrings #-}
module Aws.SimpleDb.Info
where

import           Aws.Http
import qualified Data.Ascii as A

data SdbInfo
    = SdbInfo {
        sdbiProtocol :: Protocol
      , sdbiHttpMethod :: Method
      , sdbiHost :: A.Ascii
      , sdbiPort :: Int
      }
    deriving (Show)
             
sdbUsEast :: A.Ascii
sdbUsEast = "sdb.amazonaws.com" 

sdbUsWest :: A.Ascii
sdbUsWest = "sdb.us-west-1.amazonaws.com"

sdbEuWest :: A.Ascii
sdbEuWest = "sdb.eu-west-1.amazonaws.com"

sdbApSoutheast :: A.Ascii
sdbApSoutheast = "sdb.ap-southeast-1.amazonaws.com"

sdbApNortheast :: A.Ascii
sdbApNortheast = "sdb.ap-northeast-1.amazonaws.com"
             
sdbHttpGet :: A.Ascii -> SdbInfo
sdbHttpGet endpoint = SdbInfo HTTP Get endpoint (defaultPort HTTP)
                          
sdbHttpPost :: A.Ascii -> SdbInfo
sdbHttpPost endpoint = SdbInfo HTTP PostQuery endpoint (defaultPort HTTP)
              
sdbHttpsGet :: A.Ascii -> SdbInfo
sdbHttpsGet endpoint = SdbInfo HTTPS Get endpoint (defaultPort HTTPS)
             
sdbHttpsPost :: A.Ascii -> SdbInfo
sdbHttpsPost endpoint = SdbInfo HTTPS PostQuery endpoint (defaultPort HTTPS)
