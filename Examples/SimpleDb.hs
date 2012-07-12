import qualified Aws
import qualified Aws.SimpleDb      as Sdb
import           Data.Attempt
import           Control.Exception
import qualified Data.Text         as T
import qualified Data.Text.IO      as T

main :: IO ()
main = do
  {- Load configuration -}
  cfg <- Aws.baseConfiguration
  let sdbCfg = Aws.defaultConfiguration

  putStrLn "Making request..."

  {- Make request -}
  let req = Sdb.listDomains { Sdb.ldMaxNumberOfDomains = Just 10 }
  Aws.Response _metadata resp <- Aws.simpleAws cfg sdbCfg req
  
  {- Analyze response -}
  case resp of
    Success (Sdb.ListDomainsResponse names _token) -> do
      putStrLn "First 10 domains:"
      mapM_ (T.putStrLn . T.cons '\t') names
    Failure err -> print (toException err)
