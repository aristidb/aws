module Aws.Debug
where
  
debugPrint :: (Show a) => String -> a -> IO ()
debugPrint p v = putStrLn $ "AWS Debug: " ++ p ++ " - " ++ show v