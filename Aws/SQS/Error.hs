{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
module Aws.SQS.Error
where

import           Aws.Metadata
import           Aws.SQS.Metadata
import           Aws.Xml
import           Data.Typeable
import qualified Control.Exception  as C
import qualified Network.HTTP.Types as HTTP

type ErrorCode = String

data SqsError
    = SqsError {
        sqsStatusCode :: HTTP.Status
      , sqsErrorCode :: ErrorCode
      , sqsErrorMessage :: String
      , sqsErrorMetadata :: Maybe SqsMetadata
      }
    | SqsXmlError { 
        sqsXmlErrorMessage :: String
      , sqsXmlErrorMetadata :: Maybe SqsMetadata
      }
    deriving (Show, Typeable)

--instance FromXmlError SqsError where
--    fromXmlError = flip SqsXmlError Nothing

instance WithMetadata SqsError SqsMetadata where
    getMetadata SqsError { sqsErrorMetadata = err }       = err
    getMetadata SqsXmlError { sqsXmlErrorMetadata = err } = err

    setMetadata m e@SqsError{}    = e { sqsErrorMetadata = Just m }
    setMetadata m e@SqsXmlError{} = e { sqsXmlErrorMetadata = Just m }

sqsForce :: String -> [a] -> Either SqsError a
sqsForce msg = force (SqsXmlError msg Nothing)

--instance Error SqsError where
--    noMsg = fromXmlError noMsg
--    strMsg = fromXmlError . strMsg

instance C.Exception SqsError
