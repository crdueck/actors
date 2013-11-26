module Address where

data Address = Address
    { protocol :: String
    , system   :: String
    , host     :: String
    , port     :: Int
    } deriving (Eq)

instance Show Address where
    show (Address proto sys host port) =
        proto ++ "://" ++ sys ++ "@" ++ host ++ ":" ++ show port
