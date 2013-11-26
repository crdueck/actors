module ActorRef where

import ActorPath

import Control.Concurrent.STM
import Data.Dynamic
import Data.Word

data ActorRef = ActorRef
    { (!)  :: Dynamic -> STM ()
    , (?)  :: Dynamic -> STM Dynamic
    , path :: ActorPath
    , uuid :: Word32
    }

instance Show ActorRef where
    show = show . path

instance Eq ActorRef where
    a1 == a2 = uuid a1 == uuid a2

instance Ord ActorRef where
    a1 `compare` a2 = uuid a1 `compare` uuid a2
