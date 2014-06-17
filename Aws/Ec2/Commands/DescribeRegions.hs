{-# LANGUAGE OverloadedStrings #-}
-- | The DescribeRegions operation from the AWS EC2 API.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html> 
module Aws.Ec2.Commands.DescribeRegions where

import           Data.Text           (Text)
import           Data.Typeable
import           Text.XML.Cursor                (($//), (&|))
import qualified Text.XML.Cursor                as Cu

import           Aws.Core
import           Aws.Ec2.Core

-- | Describe AWS EC2 regions matching specified criteria.
data DescribeRegions
    = DescribeRegions {
        drRegionNames :: [Text]
      , drFilters :: [RegionFilter]
      }
    deriving (Eq, Ord, Show, Typeable)

-- | Default 'DescribeRegions' query.
--
-- Unmodified, this query will list all regions accessible to the
-- authenticated account.
describeRegions :: DescribeRegions
describeRegions = DescribeRegions [] []

-- | Filters to restrict a 'DescribeRegions' query.
data RegionFilter
    = RegionNameFilter Text -- ^ Match against the region-name field.
    | EndpointFilter Text -- ^ Match against the endpoint field.
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery DescribeRegions where
  type ServiceConfiguration DescribeRegions = Ec2Configuration

  -- TODO: pass name and filters into the query.
  signQuery DescribeRegions{..} = ec2Action "DescribeRegions" []

-- ------------------------------------------------------------------ --
-- Responses

-- | Response to a 'DescribeRegions' query.
data DescribeRegionsResponse
    = DescribeRegionsResponse {
        drrRegions :: [Ec2Region] -- ^ List of 'Ec2Region's.
      }

instance ResponseConsumer r DescribeRegionsResponse where
    type ResponseMetadata DescribeRegionsResponse = Ec2Metadata

    responseConsumer _
        = ec2ResponseConsumer $ \cursor -> do
            drrRegions <- sequence $ cursor $// Cu.laxElement "item" &| parseRegion
            return DescribeRegionsResponse{..}

instance AsMemoryResponse DescribeRegionsResponse where
    type MemoryResponse DescribeRegionsResponse = DescribeRegionsResponse
    loadToMemory = return

-- ------------------------------------------------------------------ --
-- Transactions

instance Transaction DescribeRegions DescribeRegionsResponse
