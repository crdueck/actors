module ActorSystem where

import ActorRef
import ActorPath

import Control.Concurrent
import Control.Concurrent.STM
import Data.Dynamic
import Data.Unique
import System.IO.Unsafe
import qualified Data.Map as Map

type Mailbox = TQueue Dynamic

data ActorSystem = ActorSystem
    { deadletters :: ActorRef
    , register    :: ActorRef -> Mailbox -> IO ()
    , rootPath    :: ActorPath
    , shutdown    :: IO ()
    , terminated  :: IO Bool
    , sysGuardian :: ActorRef
    , usrGuardian :: ActorRef
    }

theDeadletters :: ActorRef
{-# NOINLINE theDeadletters #-}
theDeadletters = ActorRef
    { (!)  = const $ return ()
    , path = undefined
    , uuid = unsafePerformIO newUnique
    }

newActorSystem :: String -> IO ActorSystem
newActorSystem name = do
    terminated <- newEmptyMVar
    registry   <- newTVarIO Map.empty
    return $ ActorSystem
        { deadletters = theDeadletters
        , register    = ((atomically . modifyTVar registry) .) . Map.insert
        , rootPath    = undefined
        , shutdown    = putMVar terminated () -- TODO
        , terminated  = fmap not $ isEmptyMVar terminated
        , sysGuardian = undefined
        , usrGuardian = undefined
        }
