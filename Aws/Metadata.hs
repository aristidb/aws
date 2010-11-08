module Aws.Metadata
where
  
data Metadata
    = NoMetadata
    | FromSdbMetadata SdbMetadata
    deriving (Show)

data SdbMetadata 
    = SdbMetadata {
        requestId :: String
      , boxUsage :: Maybe String
      }
    deriving (Show)

class WithMetadata a where
    getMetadata :: a -> Metadata
    setMetadata :: Metadata -> a -> a

class SpecificMetadata a where
    asMetadata :: a -> Metadata
    fromMetadata :: Metadata -> Maybe a

instance SpecificMetadata () where
    asMetadata () = NoMetadata
    fromMetadata NoMetadata = Just ()
    fromMetadata _          = Nothing

instance SpecificMetadata SdbMetadata where
    asMetadata = FromSdbMetadata
    fromMetadata (FromSdbMetadata m) = Just m
    fromMetadata _                   = Nothing

getMetadata' :: (WithMetadata a, SpecificMetadata m) => a -> Maybe m
getMetadata' = fromMetadata . getMetadata

setMetadata' :: (WithMetadata a, SpecificMetadata m) => m -> a -> a
setMetadata' = setMetadata . asMetadata
