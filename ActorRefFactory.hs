module ActorRefFactory where

import Actor
import ActorContext
import ActorPath
import ActorRef
import ActorSystem
import ExecutionContext

import Prelude hiding ((/))
import Control.Concurrent.STM
import Data.Dynamic
import Data.IORef
import Data.Unique

class ActorRefFactory factory where
    actorOf     :: factory -> Actor -> String -> IO ActorRef
    dispatcher  :: factory -> ExecutionContext

instance ActorRefFactory ActorSystem where
    actorOf system actor name = do
        sender  <- fmap readIORef . newIORef $ deadletters system
        mailbox <- newTQueueIO
        uuid    <- newUnique

        let ctx = newActorContext (usrGuardian system) ref sender system
            ref = ActorRef
                { (!)  = atomically . writeTQueue mailbox . toDyn
                , path = rootPath system / name
                , uuid = uuid
                }

        register system ref mailbox
        dispatcher system `offer` runActor actor ctx mailbox
        return ref

    dispatcher = const globalExecutionContext

instance ActorRefFactory ActorContext where
    actorOf context actor name = do
        sender  <- fmap readIORef . newIORef . deadletters $ system context
        mailbox <- newTQueueIO
        uuid    <- newUnique

        let ctx = newActorContext (self context) ref sender (system context)
            ref = ActorRef
                { (!)  = atomically . writeTQueue mailbox . toDyn
                , path = path (self context) / name
                , uuid = uuid
                }

        register (system context) ref mailbox
        dispatcher context `offer` runActor actor ctx mailbox
        return ref

    -- TODO: use sub-context of global context
    dispatcher = const globalExecutionContext
