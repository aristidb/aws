{-# LANGUAGE OverloadedStrings #-}
module Aws.Ses.Info
    ( SesInfo(..)
    , sesUsEast
    , sesHttpsGet
    , sesHttpsPost
    ) where

import           Aws.Http
import qualified Data.ByteString as B

data SesInfo
    = SesInfo {
        sesiHttpMethod :: Method
      , sesiHost       :: B.ByteString
      }
    deriving (Show)

sesUsEast :: B.ByteString
sesUsEast = "email.us-east-1.amazonaws.com"

sesHttpsGet :: B.ByteString -> SesInfo
sesHttpsGet endpoint = SesInfo Get endpoint

sesHttpsPost :: B.ByteString -> SesInfo
sesHttpsPost endpoint = SesInfo PostQuery endpoint
