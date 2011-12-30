{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
module Aws.Transaction
where
  
import Aws.Response
import Aws.Signature
import Data.Monoid

class (SignQuery r, ResponseConsumer r a, Monoid (ResponseMetadata a))
    => Transaction r a | r -> a, a -> r
