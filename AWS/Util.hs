module AWS.Util
where
  
import Data.Maybe
import Data.Time
import System.Locale

fmtTime :: String -> UTCTime -> String
fmtTime = formatTime defaultTimeLocale

fmtRfc822Time :: UTCTime -> String
fmtRfc822Time = fmtTime "%a, %_d %b %Y %H:%M:%S GMT"

fmtAmzTime :: UTCTime -> String
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"

orElse :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
orElse a b = do
  x <- a
  if (isNothing x) then b else return x
