{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
module Aws.Transaction
where
  
import           Aws.Credentials
import           Aws.Query
import           Aws.Response
import           Control.Monad.IO.Class
import qualified Control.Failure         as F
import qualified Control.Monad.CatchIO   as C
import qualified Network.HTTP.Enumerator as HTTP

class (AsQuery request info, FromResponse response error, C.Exception error) 
    => Transaction request info response error | request -> response, request -> info, response -> request, response -> error

transact' :: (MonadIO io, F.Failure HTTP.HttpException io, Transaction request info response error) 
            => TimeInfo -> Credentials -> info -> request -> io (Either error response)
transact' ti cr i r = do
  q <- signQuery ti cr $ asQuery i r
  let httpRequest = queryToHttpRequest q
  rsp <- HTTP.httpLbsRedirect httpRequest
  return $ fromResponse rsp

transact :: (MonadIO io, F.Failure HTTP.HttpException io, F.Failure error io, Transaction request info response error)
            => TimeInfo -> Credentials -> info -> request -> io response
transact ti cr i r = do
  t <- transact' ti cr i r
  case t of
    Left err -> F.failure err
    Right x -> return x
