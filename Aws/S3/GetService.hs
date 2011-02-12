{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Aws.S3.GetService
where
  
import Aws.S3.Query
import Aws.Signature
import Data.ByteString.Lazy.Char8 ({- IsString -})

data GetService = GetService

instance SignQuery GetService where
    type Info GetService = () -- TODO
    signQuery GetService = s3SignQuery ()
