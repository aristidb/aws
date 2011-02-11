{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, OverloadedStrings #-}
module Aws.Transaction
where
  
import           Aws.Credentials
import           Aws.Debug
import           Aws.Query
import           Aws.Response
import           Aws.Signature
import           Data.Maybe
import qualified Data.Enumerator         as En
import qualified Network.HTTP.Enumerator as HTTP

class (SignQuery r, ResponseIteratee a)
    => Transaction r a | r -> a, a -> r

transact :: (Transaction r a) 
            => TimeInfo -> Credentials -> Info r -> r -> IO a
transact ti cr i r = do
  sd <- signatureData ti cr
  let q = signQuery i r sd
  debugPrint "String to sign" $ stringToSign q
  let httpRequest = queryToHttpRequest q
  En.run_ $ HTTP.httpRedirect httpRequest responseIteratee
