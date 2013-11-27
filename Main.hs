import Actor
import ActorContext
import ActorRef
import ActorRefFactory
import ActorSystem

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Concurrent.STM
import Data.Dynamic

echo :: Actor
echo = Actor $ \msg -> do
    ctx <- ask
    liftIO $ do
        ref <- actorOf ctx speak "speak"
        print (fromDynamic msg :: Maybe String)
        ref ! "world"

speak :: Actor
speak = Actor $ \msg -> liftIO $ print (fromDynamic msg :: Maybe String)

main = do
    sys <- newActorSystem "test"
    a1  <- actorOf sys echo "echo1"
    a1 ! "hello"
