module ActorContext where

import ActorSystem
import ActorRef

import qualified Data.Set as Set

data ActorContext = ActorContext
    { children :: Set.Set ActorRef
    , parent   :: ActorRef
    , self     :: ActorRef
    , sender   :: IO ActorRef
    , system   :: ActorSystem
    }

instance Eq ActorContext where
    c1 == c2 = self c1 == self c2

newActorContext :: ActorRef -> ActorRef -> IO ActorRef -> ActorSystem -> ActorContext
newActorContext = ActorContext Set.empty
