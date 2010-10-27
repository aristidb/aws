{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Aws.Transaction
where
  
import Aws.Credentials
import Aws.Http
import Aws.Query
import Aws.Response
import Control.Applicative
import Text.XML.Monad

class (AsQuery request info, FromResponse response error) 
    => Transaction request info response error | request -> response, request -> info, response -> request, response -> error

transact :: (Transaction request info response error) 
            => (HttpRequest -> IO HttpResponse) -> TimeInfo -> Credentials -> info -> request -> IO (Either error response)
transact http ti cr i r = do
  q <- signQuery ti cr $ asQuery i r
  let httpRequest = queryToRequest q
  rsp <- Response <$> http httpRequest
  return $ runXml rsp fromResponse
  