{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, RecordWildCards #-}
module Aws.S3.Error
where

import           Aws.Metadata
import           Aws.S3.Metadata
import           Aws.Xml
import           Data.Typeable
import qualified Control.Exception  as C
import qualified Data.ByteString    as B
import qualified Network.HTTP.Types as HTTP

type ErrorCode = String

data S3Error metadata
    = S3Error {
        s3StatusCode :: HTTP.Status
      , s3ErrorCode :: ErrorCode -- Error/Code
      , s3ErrorMessage :: String -- Error/Message
      , s3ErrorResource :: Maybe String -- Error/Resource
      , s3ErrorHostId :: Maybe String -- Error/HostId
      , s3ErrorAccessKeyId :: Maybe String -- Error/AWSAccessKeyId
      , s3ErrorStringToSign :: Maybe B.ByteString -- Error/StringToSignBytes (hexadecimal encoding)
      , s3ErrorMetadata :: metadata
      }
    | S3XmlError { 
        s3XmlErrorMessage :: String
      , s3XmlErrorMetadata :: metadata
      }
    deriving (Show, Typeable)

instance (Typeable metadata, Show metadata) => C.Exception (S3Error metadata)

instance WithMetadata S3Error where
    putMetadata m S3Error{..}    = let s3ErrorMetadata = m    in S3Error{..}
    putMetadata m S3XmlError{..} = let s3XmlErrorMetadata = m in S3XmlError{..}

s3Force :: String -> [a] -> Either (S3Error ()) a
s3Force msg = force (S3XmlError msg ())

s3ForceM :: String -> [Either (S3Error ()) a] -> Either (S3Error ()) a
s3ForceM msg = forceM (S3XmlError msg ())
