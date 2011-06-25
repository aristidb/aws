module Aws.Metadata
where
  
class Metadata m where
    emptyMetadata :: m

class WithMetadata a where
    putMetadata :: m -> a () -> a m
