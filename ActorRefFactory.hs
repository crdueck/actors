module ActorRefFactory where

import Actor
import ActorContext
import ActorRef
import ActorSystem
import ExecutionContext

class ActorRefFactory factory where
    actorOf     :: factory -> Actor -> String -> IO ActorRef
    dispatcher  :: factory -> ExecutionContext

instance ActorRefFactory ActorSystem where
    actorOf system actor name = do
        (ref, mbox) <- newActorRef system name
        ctx <- newActorContext (userGuardian system) ref system
        dispatcher system `offer` runActor actor ctx mbox
        return ref

    dispatcher = const globalExecutionContext

instance ActorRefFactory ActorContext where
    actorOf context actor name = do
        (ref, mbox) <- newActorRef (system context) name
        ctx <- newActorContext (self context) ref (system context)
        dispatcher context `offer` runActor actor ctx mbox
        return ref

    -- TODO: use sub-context of global context
    dispatcher = const globalExecutionContext
