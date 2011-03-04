{-# LANGUAGE DeriveDataTypeable #-}
module Aws.S3.Error
where

import Data.Typeable
import qualified Control.Exception as C
  
data S3Error
    = S3MissingRequestId
    | S3MissingAmz2Id
    deriving (Show, Typeable)

instance C.Exception S3Error
