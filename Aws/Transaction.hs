{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
module Aws.Transaction
where
  
import Aws.Response
import Aws.Signature
import Data.Monoid

class (SignQuery r, ResponseIteratee a, Monoid (ResponseMetadata a))
    => Transaction r a | r -> a, a -> r
