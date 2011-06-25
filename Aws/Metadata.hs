module Aws.Metadata
where
  
class WithMetadata a where
    putMetadata :: m -> a () -> a m
