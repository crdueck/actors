module ActorSystem where

import ActorRef

data ActorSystem = ActorSystem
    { deadletters :: ActorRef
    , newActorRef :: IO ActorRef
    , terminated  :: IO Bool
    , shutdown    :: IO ()
    }
