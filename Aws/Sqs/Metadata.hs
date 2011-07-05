module Aws.Sqs.Metadata where

import           Control.Monad
import           Data.Monoid
import qualified Data.Text     as T

data SqsMetadata
    = SqsMetadata {
        sqsMAmzId2 :: Maybe T.Text
      , sqsMRequestId :: Maybe T.Text
      }
    deriving (Show)

instance Monoid SqsMetadata where
    mempty = SqsMetadata Nothing Nothing
    SqsMetadata a1 r1 `mappend` SqsMetadata a2 r2 = SqsMetadata (a1 `mplus` a2) (r1 `mplus` r2)
