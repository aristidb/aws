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

orElse :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
orElse a b = a >>= \x -> if (isNothing x) then b else return x
