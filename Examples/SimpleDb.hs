import qualified Aws
import qualified Aws.SimpleDb      as Sdb
import qualified Data.Text         as T
import qualified Data.Text.IO      as T

main :: IO ()
main = do
  {- Load configuration -}
  cfg <- Aws.baseConfiguration
  let sdbCfg = Aws.defServiceConfig

  putStrLn "Making request..."

  {- Make request -}
  let req = Sdb.listDomains { Sdb.ldMaxNumberOfDomains = Just 10 }
  Sdb.ListDomainsResponse names _token <- Aws.simpleAws cfg sdbCfg req
  
  {- Analyze response -}
  putStrLn "First 10 domains:"
  mapM_ (T.putStrLn . T.cons '\t') names
