module ActorRefFactory where

import Actor
import ActorContext
import ActorRef
import ActorSystem
import ExecutionContext

import Control.Concurrent.STM
import qualified Data.Set as Set

class ActorRefFactory factory where
    actorOf     :: factory -> Actor -> String -> IO ActorRef
    dispatcher  :: factory -> ExecutionContext

instance ActorRefFactory ActorSystem where
    actorOf system actor name = do
        sender      <- newTVarIO $ deadletters system
        (ref, mbox) <- newActorRef system name
        let ctx = ActorContext Set.empty undefined ref system sender
        dispatcher system `offer` runActor actor ctx mbox
        return ref

    dispatcher = const globalExecutionContext

instance ActorRefFactory ActorContext where
    actorOf context actor name = do
        sender      <- newTVarIO . deadletters $ system context
        (ref, mbox) <- newActorRef (system context) name
        let ctx = ActorContext Set.empty (self context) ref (system context) sender
        dispatcher context `offer` runActor actor ctx mbox
        return ref

    -- TODO: use sub-context of global context
    dispatcher = const globalExecutionContext
