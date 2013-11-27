module ActorSystem where

import ActorRef
import ActorPath

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.Dynamic
import Data.Unique
import System.IO.Unsafe
import qualified Data.Map as M

type Mailbox = TQueue Dynamic
type ConcurrentMap k v = TVar (M.Map k v)

data ActorSystem = ActorSystem
    { deadletters    :: ActorRef
    , newActorRef    :: String -> IO (ActorRef, Mailbox)
    , registry       :: ConcurrentMap ActorRef Mailbox
    , rootPath       :: ActorPath
    , userGuardian   :: ActorRef
    , systemGuardian :: ActorRef
    , shutdown       :: IO ()
    , terminated     :: IO Bool
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
    registry   <- newTVarIO M.empty

    let newActorRef name = do
            mbox <- newTQueueIO
            uuid <- newUnique
            let ref = ActorRef
                    { (!)  = atomically . writeTQueue mbox . toDyn
                    , path = undefined
                    , uuid = uuid
                    }
            atomically . modifyTVar registry $ M.insert ref mbox
            return (ref, mbox)

    return $ ActorSystem
        { deadletters    = theDeadletters
        , newActorRef    = newActorRef
        , registry       = registry
        , rootPath       = undefined
        , userGuardian   = undefined
        , systemGuardian = undefined
        , shutdown       = putMVar terminated () -- TODO
        , terminated     = not <$> isEmptyMVar terminated
        }

