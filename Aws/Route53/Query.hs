{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-} 

module Aws.Route53.Query
    ( route53SignQuery
    ) where

import           Aws.Core
import           Aws.Route53.Info
import qualified Text.XML                       as XML
import qualified Data.ByteString                as B
import qualified Network.HTTP.Types             as HTTP
import qualified Network.HTTP.Conduit           as HTTP

route53SignQuery :: Method -> B.ByteString -> [(B.ByteString, B.ByteString)] -> Maybe XML.Element -> Route53Info -> SignatureData -> SignedQuery
route53SignQuery method resource query body Route53Info{..} sd
    = SignedQuery {
        sqMethod        = method
      , sqProtocol      = route53Protocol 
      , sqHost          = route53Endpoint
      , sqPort          = route53Port
      , sqPath          = route53ApiVersion `B.append` resource
      , sqQuery         = HTTP.simpleQueryToQuery query'
      , sqDate          = Just $ signatureTime sd
      , sqAuthorization = Nothing
      , sqContentType   = Nothing
      , sqContentMd5    = Nothing
      , sqAmzHeaders    = [("X-Amzn-Authorization", authorization)]
      , sqOtherHeaders  = []
      , sqBody          = renderBody `fmap` body
      , sqStringToSign  = stringToSign
      }
    where
      stringToSign  = fmtRfc822Time (signatureTime sd)
      credentials   = signatureCredentials sd
      accessKeyId   = accessKeyID credentials
      authorization = B.concat [ "AWS3-HTTPS AWSAccessKeyId="
                               , accessKeyId
                               , ", Algorithm=HmacSHA256, Signature="
                               , signature credentials HmacSHA256 stringToSign
                               ]
      query' = ("AWSAccessKeyId", accessKeyId) : query

      renderBody b = HTTP.RequestBodyLBS . XML.renderLBS XML.def $ XML.Document 
                     { XML.documentPrologue = XML.Prologue [] Nothing []
                     , XML.documentRoot = b
                     , XML.documentEpilogue = []
                     }
