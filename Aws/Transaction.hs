{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, OverloadedStrings #-}
module Aws.Transaction
where
  
import Aws.Response
import Aws.Signature

class (SignQuery r, ResponseIteratee a)
    => Transaction r a | r -> a, a -> r
