module AWS.Credentials
where
  
data Credentials
    = Credentials {
        accessKeyID :: String
      , secretAccessKey :: String
      }
    deriving (Show)