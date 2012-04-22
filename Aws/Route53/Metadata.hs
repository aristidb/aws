{-# LANGUAGE DeriveDataTypeable #-}
module Aws.Route53.Metadata
    ( Route53Metadata(..)
    ) where

import           Control.Monad
import           Data.Monoid
import           Data.Typeable
import qualified Data.Text     as T

data Route53Metadata = Route53Metadata 
    { requestId :: Maybe T.Text
    } deriving (Show, Typeable)

instance Monoid Route53Metadata where
    mempty = Route53Metadata Nothing
    Route53Metadata r1 `mappend` Route53Metadata r2 = Route53Metadata (r1 `mplus` r2)

