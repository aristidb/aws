{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Aws.SimpleDb.Error
where

import           Aws.Id
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
      , sdbErrorMetadata :: Metadata
      }
    | SdbXmlError { 
        fromSdbXmlError :: XmlError
      , sdbXmlErrorMetadata :: Metadata
      }
    deriving (Show, Typeable)

instance FromXmlError SdbError where
    fromXmlError = flip SdbXmlError NoMetadata

instance WithMetadata SdbError where
    getMetadata SdbError { sdbErrorMetadata = err }       = err
    getMetadata SdbXmlError { sdbXmlErrorMetadata = err } = err

    setMetadata m e@SdbError{}    = e { sdbErrorMetadata = m }
    setMetadata m e@SdbXmlError{} = e { sdbXmlErrorMetadata = m }

instance Error SdbError where
    noMsg = fromXmlError noMsg
    strMsg = fromXmlError . strMsg

instance C.Exception SdbError
