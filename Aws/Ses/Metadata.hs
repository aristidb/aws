{-# LANGUAGE DeriveDataTypeable #-}
module Aws.Ses.Metadata
    ( SesMetadata(..)
    ) where

import           Control.Monad
import           Data.Monoid
import           Data.Typeable
import qualified Data.Text     as T

data SesMetadata
    = SesMetadata {
        requestId :: Maybe T.Text
      }
    deriving (Show, Typeable)

instance Monoid SesMetadata where
    mempty = SesMetadata Nothing
    SesMetadata r1 `mappend` SesMetadata r2 = SesMetadata (r1 `mplus` r2)
