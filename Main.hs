{-# LANGUAGE ScopedTypeVariables #-}

import Actor
import ActorRef
import ActorRefFactory
import ActorSystem

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.STM
import Data.Dynamic
import qualified Data.Foldable as F
import qualified Data.Vector   as V

import Data.Typeable
import System.IO.Unsafe
import System.Random
import System.Exit

simple :: Actor
simple = Actor . const $ return ()

echo :: Actor
echo = Actor $ liftIO . print

gossip :: Int -> MVar (V.Vector ActorRef) -> Actor
gossip factor t = Actor $ \msg -> liftIO $ do
    rands <- replicateM factor $ randomRIO (0, size - 1)
    peers <- readMVar t
    mapM_ (\i -> peers `V.unsafeIndex` i ! i) rands

size :: Int
size = 100000

test :: Actor
test = Actor $ \dyn ->
    F.forM_ (fromDynamic dyn :: Maybe Bool) $ \b ->
        become $ if b then intRecv else strRecv
    where intRecv msg = liftIO (F.forM_ (fromDynamic msg :: Maybe Int) print) >> unbecome
          strRecv msg = liftIO (F.forM_ (fromDynamic msg :: Maybe String) putStrLn) >> unbecome

main = do
    sys <- newActorSystem "sys"
    t1  <- actorOf sys test "test"
    t1 ! True
    t1 ! (0 :: Int)
    t1 ! "hello"

{-main = do-}
    {-system <- newActorSystem "test"-}
    {-peers  <- newEmptyMVar-}
    {-refs   <- V.replicateM size $ actorOf system (gossip 5 peers) ""-}
    {-putMVar peers refs-}
    {-V.head refs ! "start"-}
    {-threadDelay 10000000-}
    {-exitSuccess-}
