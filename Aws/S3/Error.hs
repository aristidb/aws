{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
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

data S3Error
    = S3Error {
        s3StatusCode :: HTTP.Status
      , s3ErrorCode :: ErrorCode -- Error/Code
      , s3ErrorMessage :: String -- Error/Message
      , s3ErrorResource :: Maybe String -- Error/Resource
      , s3ErrorHostId :: Maybe String -- Error/HostId
      , s3ErrorAccessKeyId :: Maybe String -- Error/AWSAccessKeyId
      , s3ErrorStringToSign :: Maybe B.ByteString -- Error/StringToSignBytes (hexadecimal encoding)
      , s3ErrorMetadata :: Maybe S3Metadata
      }
    | S3XmlError { 
        s3XmlErrorMessage :: String
      , s3XmlErrorMetadata :: Maybe S3Metadata
      }
    deriving (Show, Typeable)

instance C.Exception S3Error

instance WithMetadata S3Error S3Metadata where
    getMetadata S3Error { s3ErrorMetadata = m } = m
    getMetadata S3XmlError { s3XmlErrorMetadata = m } = m

    setMetadata m a@S3Error{} = a { s3ErrorMetadata = Just m }
    setMetadata m a@S3XmlError{} = a { s3XmlErrorMetadata = Just m }

s3Force :: String -> [a] -> Either S3Error a
s3Force msg = force (S3XmlError msg Nothing)

s3ForceM :: String -> [Either S3Error a] -> Either S3Error a
s3ForceM msg = forceM (S3XmlError msg Nothing)
