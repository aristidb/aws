{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
module Aws.SimpleDb.Error
where

import           Aws.Metadata
import           Control.Monad.Error.Class
import           Data.Typeable
import           Text.XML.Monad
import qualified Control.Exception         as C

type ErrorCode = String

data SdbError
    = SdbError {
        sdbStatusCode :: Int
      , sdbErrorCode :: ErrorCode
      , sdbErrorMessage :: String
      , sdbErrorMetadata :: Maybe SdbMetadata
      }
    | SdbXmlError { 
        fromSdbXmlError :: XmlError
      , sdbXmlErrorMetadata :: Maybe SdbMetadata
      }
    deriving (Show, Typeable)

instance FromXmlError SdbError where
    fromXmlError = flip SdbXmlError Nothing

instance WithMetadata SdbError SdbMetadata where
    getMetadata SdbError { sdbErrorMetadata = err }       = err
    getMetadata SdbXmlError { sdbXmlErrorMetadata = err } = err

    setMetadata m e@SdbError{}    = e { sdbErrorMetadata = Just m }
    setMetadata m e@SdbXmlError{} = e { sdbXmlErrorMetadata = Just m }

instance Error SdbError where
    noMsg = fromXmlError noMsg
    strMsg = fromXmlError . strMsg

instance C.Exception SdbError
