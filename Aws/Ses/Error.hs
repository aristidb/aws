{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, RecordWildCards #-}
module Aws.Ses.Error
    ( SesError(..)
    ) where

import           Data.Typeable
import           Data.Text                 (Text)
import qualified Control.Exception         as C
import qualified Network.HTTP.Types        as HTTP


data SesError
    = SesError {
        sesStatusCode   :: HTTP.Status
      , sesErrorCode    :: Text
      , sesErrorMessage :: Text
      }
    deriving (Show, Typeable)

instance C.Exception SesError
