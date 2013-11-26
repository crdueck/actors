import Actor
import ActorRef
import ActorSystem

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM

echo :: Actor
echo = Actor $ \msg -> do
    ctx <- ask
    lift . lift $ do
        rep <- readTVar $ sender ctx
        rep ! msg

main = do
    system <- newActorSystem "test"
    actorOf system echo
