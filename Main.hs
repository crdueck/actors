import Actor
import ActorRef
import ActorRefFactory
import ActorSystem

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import System.Random
import System.Exit

import qualified Data.Vector as V

echo :: Actor
echo = Actor $ liftIO . print

simple :: Actor
simple = Actor . const $ return ()

gossip :: Int -> MVar (V.Vector ActorRef) -> Actor
gossip factor t = Actor $ \msg -> liftIO $ do
    rands <- replicateM factor $ randomRIO (0, size - 1)
    peers <- readMVar t
    mapM_ (\i -> peers `V.unsafeIndex` i ! i) rands

size :: Int
size = 100000

main = do
    system <- newActorSystem "test"
    peers  <- newEmptyMVar
    refs   <- V.replicateM size $ actorOf system (gossip 5 peers) ""
    putMVar peers refs
    V.head refs ! "start"
    threadDelay 10000000
    exitSuccess
