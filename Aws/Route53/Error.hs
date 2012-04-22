{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, RecordWildCards #-}
module Aws.Route53.Error
    ( Route53Error(..)
    ) where

import           Data.Typeable
import           Data.Text                 (Text)
import qualified Control.Exception         as C
import qualified Network.HTTP.Types        as HTTP

-- TODO route53 documentation seem to indicate that there is also a type field in the error response body.
-- http://docs.amazonwebservices.com/Route53/latest/DeveloperGuide/ResponseHeader_RequestID.html

data Route53Error = Route53Error
      { route53StatusCode   :: HTTP.Status
      , route53ErrorCode    :: Text
      , route53ErrorMessage :: Text
      } deriving (Show, Typeable)

instance C.Exception Route53Error
