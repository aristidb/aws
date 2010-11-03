{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Aws.Transaction
where
  
import Aws.Credentials
import Aws.Http
import Aws.Query
import Aws.Response
import Control.Monad
import Control.Monad.IO.Class
import Text.XML.Monad

class (AsQuery request info, FromResponse response error) 
    => Transaction request info response error | request -> response, request -> info, response -> request, response -> error

transact :: MonadIO io => (Transaction request info response error) 
            => (HttpRequest -> io HttpResponse) -> TimeInfo -> Credentials -> info -> request -> io (Either error response)
transact http ti cr i r = do
  q <- signQuery ti cr $ asQuery i r
  let httpRequest = queryToRequest q
  rsp <- Response `liftM` http httpRequest
  return $ runXml fromResponse rsp
