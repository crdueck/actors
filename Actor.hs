module Actor where

import ActorContext

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Dynamic

type Mailbox = TQueue Dynamic
type Receive = Dynamic -> ReaderT ActorContext (MaybeT IO) ()

newtype Actor = Actor { receive :: Receive }

runActor :: Actor -> ActorContext -> Mailbox -> IO ()
runActor actor ctx mailbox = forever $ do
    mail <- atomically $ tryReadTQueue mailbox
    case mail of
        Nothing  -> yield
        Just msg -> process msg >>= maybe (unhandled msg) return
    where process   msg = runMaybeT $ runReaderT (actor `receive` msg) ctx
          unhandled msg = putStr "unhandled: " >> print msg
