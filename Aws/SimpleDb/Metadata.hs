{-# LANGUAGE DeriveDataTypeable #-}
module Aws.SimpleDb.Metadata
where
  
import Data.Typeable

data SdbMetadata 
    = SdbMetadata {
        requestId :: String
      , boxUsage :: Maybe String
      }
    deriving (Show, Typeable)
