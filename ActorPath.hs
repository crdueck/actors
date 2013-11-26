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

instance Eq ActorPath where
    p1 == p2 =
        address p1 == address p2 && parent p1 == parent p2 && name p1 == name p2
