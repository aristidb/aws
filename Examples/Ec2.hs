import qualified Aws
import qualified Aws.Ec2 as Ec2

main :: IO ()
main = do
  -- Load the configuration for AWS and the EC2 API.
  cfg <- Aws.baseConfiguration
  let ec2cfg = Aws.defServiceConfig

  -- Request the region list.
  let req = Ec2.describeRegions
  Ec2.DescribeRegionsResponse regions <- Aws.simpleAws cfg ec2cfg req

  -- Print the returned regions.
  mapM_ (print) regions
