-- GHCI convenience code

import           Aws
import qualified Aws.Route53 as R53
import qualified Aws.S3 as S3
import qualified Aws.Ses as Ses
import qualified Aws.SimpleDb as Sdb
import qualified Aws.Sqs as Sqs
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Default
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

import           System.IO.Unsafe -- only for the initialisation Please

bcfg = unsafePerformIO baseConfiguration
dcfg = unsafePerformIO dbgConfiguration
mgr = unsafePerformIO (HTTP.newManager def)
