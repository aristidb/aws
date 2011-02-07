{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Aws.S3.GetService
where
  
import Aws.Http
import Aws.Query
import Data.ByteString.Lazy.Char8 ({- IsString -})

data GetService = GetService

instance AsQuery GetService where
    type Info GetService = () -- < preliminary
    asQuery _ _ = Query {
                    api = S3
                  , method = Get
                  , protocol = HTTP
                  , host = "s3.amazonaws.com"
                  , port = 80
                  , path = "/"
                  , canonicalizedResource = "/"
                  , subresource = Nothing
                  , query = []
                  , date = Nothing
                  , authorization = Nothing
                  , contentType = Nothing
                  , contentMd5 = Nothing
                  , body = ""
                  , stringToSign = Nothing
                  }
