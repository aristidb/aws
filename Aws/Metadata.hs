module Aws.Metadata
where
  
class Metadata m where
    emptyMetadata :: m

instance Metadata () where
    emptyMetadata = ()

class WithMetadata a where
    putMetadata :: m -> a () -> a m
