module Aws.Debug
where
  
import Control.Monad.IO.Class

debugPrint :: (MonadIO io, Show a) => String -> a -> io ()
debugPrint p v = liftIO . putStrLn $ "AWS Debug: " ++ p ++ " - " ++ show v