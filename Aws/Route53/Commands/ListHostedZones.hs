{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Aws.Route53.Commands.ListHostedZones where

import           Aws.Response
import           Aws.Signature
import           Aws.Route53.Info
import           Aws.Route53.Model
import           Aws.Route53.Metadata
import           Aws.Route53.Query
import           Aws.Route53.Response
import           Aws.Transaction
import           Aws.Xml
import           Data.Maybe
import           Control.Applicative        ((<$>))
import           Text.XML.Cursor            (($//), (&/), laxElement)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

data ListHostedZones = ListHostedZones
                     { lhzMaxNumberOfItems :: Maybe Int
                     , lhzNextToken :: Maybe T.Text
                     } deriving (Show)

data ListHostedZonesResponse = ListHostedZonesResponse
                             { lhzrHostedZones :: [HostedZone]
                             , lhzrNextToken :: Maybe T.Text
                             } deriving (Show)

listHostedZones :: ListHostedZones
listHostedZones = ListHostedZones { lhzMaxNumberOfItems = Nothing, lhzNextToken = Nothing }

-- TODO sign the date header
instance SignQuery ListHostedZones where
    type Info ListHostedZones = Route53Info
    signQuery ListHostedZones{..} = route53SignQuery resource query
      where
      resource = "/hostedzone"
      query = catMaybes -- query info signatureData
            [ ("MaxItems",) . T.encodeUtf8 . T.pack . show <$> lhzMaxNumberOfItems
            , ("NextToken",) . T.encodeUtf8 <$> lhzNextToken
            ]

instance ResponseConsumer r ListHostedZonesResponse where
    type ResponseMetadata ListHostedZonesResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "ListHostedZonesResponse" cursor
            zones <- mapM parseHostedZone $ cursor $// laxElement "HostedZones" &/ laxElement "HostedZone"
            let nextToken = listToMaybe $ cursor $// elContent "NextMarker"
            return $ ListHostedZonesResponse zones nextToken

instance Transaction ListHostedZones ListHostedZonesResponse where

