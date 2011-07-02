{-# LANGUAGE DeriveDataTypeable #-}
module Aws.SimpleDb.Metadata
where
  
import           Control.Monad
import           Data.Monoid
import           Data.Typeable
import qualified Data.Text     as T

data SdbMetadata 
    = SdbMetadata {
        requestId :: Maybe T.Text
      , boxUsage :: Maybe T.Text
      }
    deriving (Show, Typeable)

instance Monoid SdbMetadata where
    mempty = SdbMetadata Nothing Nothing
    SdbMetadata r1 b1 `mappend` SdbMetadata r2 b2 = SdbMetadata (r1 `mplus` r2) (b1 `mplus` b2)