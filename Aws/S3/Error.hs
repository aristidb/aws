{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, RecordWildCards #-}
module Aws.S3.Error
where

import           Data.Typeable
import qualified Control.Exception  as C
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Network.HTTP.Types as HTTP

type ErrorCode = T.Text

data S3Error
    = S3Error {
        s3StatusCode :: HTTP.Status
      , s3ErrorCode :: ErrorCode -- Error/Code
      , s3ErrorMessage :: T.Text -- Error/Message
      , s3ErrorResource :: Maybe T.Text -- Error/Resource
      , s3ErrorHostId :: Maybe T.Text -- Error/HostId
      , s3ErrorAccessKeyId :: Maybe T.Text -- Error/AWSAccessKeyId
      , s3ErrorStringToSign :: Maybe B.ByteString -- Error/StringToSignBytes (hexadecimal encoding)
      }
    deriving (Show, Typeable)

instance C.Exception S3Error
