{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, RecordWildCards #-}
module Aws.SimpleDb.Error
where

import           Aws.Response
import           Aws.SimpleDb.Metadata
import           Aws.Xml
import           Data.Typeable
import qualified Control.Exception         as C
import qualified Network.HTTP.Types        as HTTP

type ErrorCode = String

data SdbError
    = SdbError {
        sdbStatusCode :: HTTP.Status
      , sdbErrorCode :: ErrorCode
      , sdbErrorMessage :: String
      }
    deriving (Show, Typeable)

instance C.Exception SdbError
