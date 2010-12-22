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

class (AsQuery r i, ResponseIteratee m a)
    => Transaction r i m a | r -> a i, a -> r

transact :: (Transaction d i io a
             , MonadIO io
             , F.Failure HTTP.HttpException io) 
            => TimeInfo -> Credentials -> i -> d -> io a
transact ti cr i r = do
  q <- signQuery ti cr $ asQuery i r
  let httpRequest = queryToHttpRequest q
  HTTP.httpRedirect responseIteratee httpRequest
