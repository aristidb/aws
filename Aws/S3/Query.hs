module Aws.S3.Query
where
  
{-
        S3 -> B.intercalate "\n" $ concat [[httpMethod method]
                                          , [fromMaybe "" contentMd5]
                                          , [fromMaybe "" contentType]
                                          , [case ti of
                                               AbsoluteTimestamp time -> fmtRfc822Time time
                                               AbsoluteExpires time -> fmtTimeEpochSeconds time]
                                          , [] -- canonicalized AMZ headers
                                          , [canonicalizedResource]]
-}

{-
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
-}