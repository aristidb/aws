{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables #-}
module Aws.Transaction
where
  
import           Aws.Credentials
import           Aws.Query
import           Aws.Response
import           Control.Monad.IO.Class
import qualified Control.Failure         as F
import qualified Control.Monad.CatchIO   as C
import qualified Network.HTTP.Enumerator as HTTP

class (AsQuery r i, ResponseIteratee a)
    => Transaction r i a | r -> a i, a -> r

transact :: (Transaction r i a) 
            => TimeInfo -> Credentials -> i -> r -> IO a
transact ti cr i r = do
  q <- signQuery ti cr $ asQuery i r
  let httpRequest = queryToHttpRequest q
  HTTP.httpRedirect responseIteratee httpRequest
