{-# LANGUAGE DeriveDataTypeable #-}
module Aws.S3.Error
where

import           Aws.Metadata
import           Data.Typeable
import           Text.XML.Monad
import           Control.Monad.Error.Class
import qualified Control.Exception         as C

data S3Error
    = S3XmlError { 
        fromS3XmlError :: XmlError
      , s3XmlErrorMetadata :: Metadata
      }
    deriving (Show, Typeable)

instance C.Exception S3Error

instance FromXmlError S3Error where
    fromXmlError = flip S3XmlError NoMetadata

instance Error S3Error where
    noMsg = fromXmlError noMsg
    strMsg = fromXmlError . strMsg

instance WithMetadata S3Error where
    getMetadata = s3XmlErrorMetadata
    setMetadata m a = a { s3XmlErrorMetadata = m }
