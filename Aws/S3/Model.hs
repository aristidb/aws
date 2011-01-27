{-# LANGUAGE OverloadedStrings #-}
module Aws.S3.Model
where
  
import qualified Data.ByteString as B

type Bucket = B.ByteString

data LocationConstraint 
    = EU | UsWest1 | ApSouthEast1 | UsClassic
    deriving (Show)

locationConstraintToId :: LocationConstraint -> B.ByteString
locationConstraintToId lc
    = case lc of
        EU -> "EU"
        UsWest1 -> "us-west-1"
        ApSouthEast1 -> "ap-southeast-1"
        UsClassic -> ""

idToLocationConstraint :: B.ByteString -> Maybe LocationConstraint
idToLocationConstraint id
    = case id of
        "EU" -> Just EU
        "us-west-1" -> Just UsWest1
        "ap-southeast-1" -> Just ApSouthEast1
        "" -> Just UsClassic
        _ -> Nothing
