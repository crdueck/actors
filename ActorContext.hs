module ActorContext where

import ActorSystem
import ActorRef

import Control.Concurrent.STM
import qualified Data.Set as Set

data ActorContext = ActorContext
    { children :: Set.Set ActorRef
    , parent   :: ActorRef
    , self     :: ActorRef
    , system   :: ActorSystem
    , sender   :: TVar ActorRef
    }

instance Eq ActorContext where
    c1 == c2 = self c1 == self c2
