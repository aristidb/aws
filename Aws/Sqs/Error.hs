{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
module Aws.Sqs.Error where

import           Aws.Sqs.Metadata
import           Data.Typeable
import qualified Control.Exception  as C
import qualified Data.Text          as T
import qualified Network.HTTP.Types as HTTP

type ErrorCode = T.Text

data SqsError
    = SqsError {
        sqsStatusCode :: HTTP.Status
      , sqsErrorCode :: ErrorCode
      , sqsErrorType :: T.Text
      , sqsErrorMessage :: T.Text
      , sqsErrorDetail :: Maybe T.Text
      , sqsErrorMetadata :: Maybe SqsMetadata
      }
    | SqsXmlError { 
        sqsXmlErrorMessage :: T.Text
      , sqsXmlErrorMetadata :: Maybe SqsMetadata
      }
    deriving (Show, Typeable)

instance C.Exception SqsError
