{-# LANGUAGE DeriveDataTypeable #-}
module Aws.SimpleDb.Metadata
where
  
import Control.Monad
import Data.Monoid
import Data.Typeable

data SdbMetadata 
    = SdbMetadata {
        requestId :: Maybe String
      , boxUsage :: Maybe String
      }
    deriving (Show, Typeable)

instance Monoid SdbMetadata where
    mempty = SdbMetadata Nothing Nothing
    SdbMetadata r1 b1 `mappend` SdbMetadata r2 b2 = SdbMetadata (r1 `mplus` r2) (b1 `mplus` b2)