module Aws.SimpleDb.Metadata
where
  
data SdbMetadata
    = SdbMetadata {
        requestId :: String
      , boxUsage :: Maybe String
      }
    deriving (Show)
