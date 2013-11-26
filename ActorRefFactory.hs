module ActorRefFactory where

import Actor
import ActorContext
import ActorRef
import ActorSystem
import ExecutionContext

import Control.Concurrent.STM
import qualified Data.Set as Set

class ActorRefFactory factory where
    actorOf     :: factory -> Actor -> IO ActorRef
    dispatcher  :: factory -> ExecutionContext

instance ActorRefFactory ActorSystem where
    actorOf system actor = do
        sender  <- newTVarIO $ deadletters system
        fresh   <- newActorRef system
        mailbox <- newTQueueIO
        let ctx = ActorContext Set.empty undefined fresh system sender
        dispatcher system `offer` runActor actor ctx mailbox
        return fresh

    dispatcher = const globalExecutionContext

instance ActorRefFactory ActorContext where
    actorOf context actor = do
        sender  <- newTVarIO . deadletters $ system context
        fresh   <- newActorRef (system context)
        mailbox <- newTQueueIO
        let ctx = ActorContext Set.empty (self context) fresh (system context) sender
        dispatcher context `offer` runActor actor ctx mailbox
        return fresh

    -- TODO: use sub-context of global context
    dispatcher = const globalExecutionContext
