module Actor where

import ActorContext

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Dynamic

type Mailbox = TQueue Dynamic
type Receive = Dynamic -> ReaderT ActorContext (MaybeT IO) ()

newtype Actor = Actor { receive :: Receive }

runActor :: Actor -> ActorContext -> Mailbox -> IO ()
runActor actor ctx mailbox = forever $ do
    msg <- atomically $ readTQueue mailbox
    runMaybeT $ actor `receive` msg `runReaderT` ctx
