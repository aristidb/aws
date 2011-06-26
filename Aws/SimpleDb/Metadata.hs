{-# LANGUAGE DeriveDataTypeable #-}
module Aws.SimpleDb.Metadata
where
  
import Aws.Response
import Data.Typeable

data SdbMetadata 
    = SdbMetadata {
        requestId :: Maybe String
      , boxUsage :: Maybe String
      }
    deriving (Show, Typeable)

instance Metadata SdbMetadata where
    emptyMetadata = SdbMetadata Nothing Nothing