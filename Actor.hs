module Actor where

import ActorRef
import ActorContext

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Dynamic

newtype Actor = Actor { receive :: Receive }

type Mailbox = TQueue Dynamic
type Receive = Dynamic -> ReaderT ActorContext (MaybeT STM) ()

runActor :: Actor -> ActorContext -> Mailbox -> IO ()
runActor actor ctx mailbox = forever $ do
    mail <- atomically $ tryReadTQueue mailbox
    case mail of
        Nothing  -> yield
        Just msg -> atomically (process msg) >>= maybe (unhandled msg) return
    where process   msg = runMaybeT $ runReaderT (actor `receive` msg) ctx
          unhandled msg = putStr "unhandled: " >> print msg

echo :: Actor
echo = Actor $ \msg -> do
    ctx <- ask
    lift . lift $ do
        rep <- readTVar $ sender ctx
        rep ! msg
