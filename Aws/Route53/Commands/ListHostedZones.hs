-- ------------------------------------------------------ --
-- Copyright © 2012 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

-- | GET ListHostedZones
--
--   List all Route53 hosted zones of the user, optionally paginated.
--
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_ListHostedZones.html>
--
module Aws.Route53.Commands.ListHostedZones where

import           Aws.Core
import           Aws.Route53.Core
import           Data.Maybe
import           Control.Applicative        ((<$>), (<$))
import           Text.XML.Cursor            (($//))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

data ListHostedZones = ListHostedZones
                     { lhzMaxNumberOfItems :: Maybe Int
                     , lhzNextToken :: Maybe T.Text
                     } deriving (Show)

data ListHostedZonesResponse = ListHostedZonesResponse
                             { lhzrHostedZones :: HostedZones
                             , lhzrNextToken :: Maybe T.Text
                             } deriving (Show)

listHostedZones :: ListHostedZones
listHostedZones = ListHostedZones { lhzMaxNumberOfItems = Nothing, lhzNextToken = Nothing }

-- | ServiceConfiguration: 'Route53Configuration'
instance SignQuery ListHostedZones where
    type ServiceConfiguration ListHostedZones = Route53Configuration
    signQuery ListHostedZones{..} = route53SignQuery method resource query Nothing
      where
      method = Get
      resource = "/hostedzone"
      query = catMaybes
            [ ("MaxItems",) . T.encodeUtf8 . T.pack . show <$> lhzMaxNumberOfItems
            , ("NextToken",) . T.encodeUtf8 <$> lhzNextToken
            ]

instance ResponseConsumer r ListHostedZonesResponse where
    type ResponseMetadata ListHostedZonesResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parser
        where 
        parser cursor = do
            route53CheckResponseType () "ListHostedZonesResponse" cursor
            zones <- r53Parse cursor
            let nextToken = listToMaybe $ cursor $// elContent "NextMarker"
            return $ ListHostedZonesResponse zones nextToken

instance Transaction ListHostedZones ListHostedZonesResponse

instance IteratedTransaction ListHostedZones ListHostedZonesResponse where
    nextIteratedRequest req ListHostedZonesResponse{ lhzrNextToken = nt } = req { lhzNextToken = nt } <$ nt
    combineIteratedResponse (ListHostedZonesResponse z0 _) (ListHostedZonesResponse z1 nt) = ListHostedZonesResponse (z0 ++ z1) nt

