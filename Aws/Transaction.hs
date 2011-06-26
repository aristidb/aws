{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
module Aws.Transaction
where
  
import Aws.Response
import Aws.Signature

class (SignQuery r, ResponseIteratee a, Metadata (ResponseMetadata a))
    => Transaction r a | r -> a, a -> r
