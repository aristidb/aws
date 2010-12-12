{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Aws.Transaction
where
  
import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.Response
import           Control.Monad
import           Control.Monad.IO.Class
import           Text.XML.Monad
import qualified Control.Monad.CatchIO  as C

class (AsQuery request info, FromResponse response error, C.Exception error) 
    => Transaction request info response error | request -> response, request -> info, response -> request, response -> error

transact' :: MonadIO io => (Transaction request info response error) 
            => (HttpRequest -> IO HttpResponse) -> TimeInfo -> Credentials -> info -> request -> io (Either error response)
transact' http ti cr i r = do
  q <- signQuery ti cr $ asQuery i r
  let httpRequest = queryToRequest q
  rsp <- liftIO $ Response `liftM` http httpRequest
  return $ fromResponse rsp

transact :: MonadIO io => (Transaction request info response error)
            => (HttpRequest -> IO HttpResponse) -> TimeInfo -> Credentials -> info -> request -> io response
transact http ti cr i r = do
  t <- transact' http ti cr i r
  case t of
    Left err -> C.throw err
    Right x -> return x
