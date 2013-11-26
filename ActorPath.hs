module ActorPath where

import Address

data ActorPath = ActorPath
    { address :: Address
    , parent  :: ActorPath
    , name    :: String
    , (/)     :: String -> ActorPath
    }

-- TODO
instance Show ActorPath where
    show a = show (parent a) ++ "/" ++ name a
