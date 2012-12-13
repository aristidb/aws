module Aws.Ec2.InstanceMetadata where

import           Control.Applicative
import           Control.Exception
import           Control.Failure
import           Control.Monad.Trans.Resource
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.ByteString.Lazy.UTF8 as BU
import           Data.Typeable
import qualified Network.HTTP.Conduit as HTTP

data InstanceMetadataException
  = MetadataNotFound String
  deriving (Show, Typeable)

instance Exception InstanceMetadataException

getInstanceMetadata :: HTTP.Manager -> String -> String -> ResIO ByteString
getInstanceMetadata mgr p x = do req <- HTTP.parseUrl ("http://169.254.169.254/" ++ p ++ '/' : x)
                                 HTTP.responseBody <$> HTTP.httpLbs req mgr

getInstanceMetadataListing :: HTTP.Manager -> String -> ResIO [String]
getInstanceMetadataListing mgr p = map BU.toString . B8.split '\n' <$> getInstanceMetadata mgr p ""

getInstanceMetadataFirst :: HTTP.Manager -> String -> ResIO ByteString
getInstanceMetadataFirst mgr p = do listing <- getInstanceMetadataListing mgr p
                                    case listing of
                                      [] -> failure (MetadataNotFound p)
                                      (x:_) -> getInstanceMetadata mgr p x

getInstanceMetadataOrFirst :: HTTP.Manager -> String -> Maybe String -> ResIO ByteString
getInstanceMetadataOrFirst mgr p (Just x) = getInstanceMetadata mgr p x
getInstanceMetadataOrFirst mgr p Nothing = getInstanceMetadataFirst mgr p
