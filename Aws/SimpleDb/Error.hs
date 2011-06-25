{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, RecordWildCards #-}
module Aws.SimpleDb.Error
where

import           Aws.Metadata
import           Aws.SimpleDb.Metadata
import           Aws.Xml
import           Data.Typeable
import qualified Control.Exception         as C
import qualified Network.HTTP.Types        as HTTP

type ErrorCode = String

data SdbError metadata
    = SdbError {
        sdbStatusCode :: HTTP.Status
      , sdbErrorCode :: ErrorCode
      , sdbErrorMessage :: String
      , sdbErrorMetadata :: metadata
      }
    | SdbXmlError { 
        sdbXmlErrorMessage :: String
      , sdbXmlErrorMetadata :: metadata
      }
    deriving (Show, Typeable)

instance WithMetadata SdbError where
    putMetadata m SdbError{..}    = let sdbErrorMetadata = m    in SdbError{..}
    putMetadata m SdbXmlError{..} = let sdbXmlErrorMetadata = m in SdbXmlError{..}

instance (Show metadata, Typeable metadata) => C.Exception (SdbError metadata)

sdbForce :: String -> [a] -> Either (SdbError ()) a
sdbForce msg = force (SdbXmlError msg ())

sdbForceM :: String -> [Either (SdbError ()) a] -> Either (SdbError ()) a
sdbForceM msg = forceM (SdbXmlError msg ())
