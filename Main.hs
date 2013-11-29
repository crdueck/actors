{-# LANGUAGE ScopedTypeVariables #-}

import Actor
import ActorRef
import ActorRefFactory
import ActorSystem

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.STM
import Data.Dynamic
import qualified Data.Foldable as F

import Data.Typeable
import System.IO.Unsafe

simple :: Actor
simple = Actor . const $ return ()

speak :: Actor
speak = Actor $ \msg -> liftIO $ print (fromDynamic msg :: Maybe String)

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
