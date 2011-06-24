{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
module Aws.SimpleDb.Error
where

import           Aws.Metadata
import           Aws.SimpleDb.Metadata
import           Aws.Xml
import           Control.Monad.Error.Class
import           Data.Typeable
import           Text.XML.Monad
import qualified Control.Exception         as C
import qualified Network.HTTP.Types        as HTTP

type ErrorCode = String

data SdbError
    = SdbError {
        sdbStatusCode :: HTTP.Status
      , sdbErrorCode :: ErrorCode
      , sdbErrorMessage :: String
      , sdbErrorMetadata :: Maybe SdbMetadata
      }
    | SdbXmlError { 
        sdbXmlErrorMessage :: String
      , sdbXmlErrorMetadata :: Maybe SdbMetadata
      }
    deriving (Show, Typeable)

instance FromXmlError SdbError where
    fromXmlError = flip SdbXmlError Nothing . show

instance WithMetadata SdbError SdbMetadata where
    getMetadata SdbError { sdbErrorMetadata = err }       = err
    getMetadata SdbXmlError { sdbXmlErrorMetadata = err } = err

    setMetadata m e@SdbError{}    = e { sdbErrorMetadata = Just m }
    setMetadata m e@SdbXmlError{} = e { sdbXmlErrorMetadata = Just m }

instance Error SdbError where
    noMsg = fromXmlError noMsg
    strMsg = fromXmlError . strMsg

instance C.Exception SdbError

sdbForce :: String -> [a] -> Either SdbError a
sdbForce msg = force (SdbXmlError msg Nothing)