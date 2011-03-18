{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Aws.Metadata
where
  
class WithMetadata a m | a -> m where
    getMetadata :: a -> Maybe m
    setMetadata :: m -> a -> a
