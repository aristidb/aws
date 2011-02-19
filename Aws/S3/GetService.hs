{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Aws.S3.GetService
where
  
import           Aws.S3.Info
import           Aws.S3.Query
import           Aws.Signature
import           Aws.Transaction
import qualified Network.HTTP.Enumerator as HTTP

data GetService = GetService

instance SignQuery GetService where
    type Info GetService = S3Info
    signQuery GetService = s3SignQuery ()

instance Transaction GetService HTTP.Response