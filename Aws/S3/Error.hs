{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, RecordWildCards #-}
module Aws.S3.Error
where

import           Aws.Response
import           Aws.S3.Metadata
import           Aws.Xml
import           Data.Typeable
import qualified Control.Exception  as C
import qualified Data.ByteString    as B
import qualified Network.HTTP.Types as HTTP

type ErrorCode = String

data S3Error
    = S3Error {
        s3StatusCode :: HTTP.Status
      , s3ErrorCode :: ErrorCode -- Error/Code
      , s3ErrorMessage :: String -- Error/Message
      , s3ErrorResource :: Maybe String -- Error/Resource
      , s3ErrorHostId :: Maybe String -- Error/HostId
      , s3ErrorAccessKeyId :: Maybe String -- Error/AWSAccessKeyId
      , s3ErrorStringToSign :: Maybe B.ByteString -- Error/StringToSignBytes (hexadecimal encoding)
      }
    deriving (Show, Typeable)

instance C.Exception S3Error
