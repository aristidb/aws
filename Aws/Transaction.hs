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

class (AsQuery r i, ResponseIteratee e m a, C.Exception e)
    => Transaction r i e m a | r -> a i, a -> r e

transact :: (Transaction d i e io a
             , MonadIO io
             , F.Failure HTTP.HttpException io
             , F.Failure e io) 
            => TimeInfo -> Credentials -> i -> d -> io a
transact ti cr i r = do
  q <- signQuery ti cr $ asQuery i r
  let httpRequest = queryToHttpRequest q
  HTTP.httpRedirect responseIteratee httpRequest
