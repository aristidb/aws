{-# LANGUAGE OverloadedStrings #-}
-- | The DescribeRegions operation from the AWS EC2 API.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html> 
module Aws.Ec2.Commands.DescribeRegions where

import           Data.Text           (Text)
import           Data.Typeable

import           Aws.Core
import           Aws.Ec2.Core

-- | Describe AWS EC2 regions matching specified criteria.
data DescribeRegions
    = DescribeRegions {
        drRegionNames :: [Text]
      , drFilters :: [RegionFilter]
      }
    deriving (Eq, Ord, Show, Typeable)

-- | Filters to restrict a 'DescribeRegions' query.
data RegionFilter
    = RegionNameFilter Text -- ^ Match against the region-name field.
    | EndpointFilter Text -- ^ Match against the endpoint field.
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery DescribeRegions where
  type ServiceConfiguration DescribeRegions = Ec2Configuration

  signQuery DescribeRegions{..} = ec2Action "DescribeRegions" []


