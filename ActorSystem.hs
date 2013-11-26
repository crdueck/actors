module ActorSystem where

import ActorRef

import Control.Concurrent
import Control.Concurrent.STM
import Data.Dynamic
import qualified Data.Map as M

type Mailbox = TQueue Dynamic
type ConcurrentMap k v = TVar (M.Map k v)

data ActorSystem = ActorSystem
    { deadletters :: ActorRef
    , newActorRef :: IO ActorRef
    , terminated  :: IO Bool
    , shutdown    :: IO ()
    , registry    :: ConcurrentMap ActorRef Mailbox
    }

foo :: IO ActorRef
foo = do undefined

newActorSystem :: String -> IO ActorSystem
newActorSystem name = do
    terminated <- newEmptyMVar
    undefined
