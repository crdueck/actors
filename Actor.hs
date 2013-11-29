{-# LANGUAGE FlexibleContexts #-}

module Actor where

import ActorContext

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import Data.Dynamic


type Mailbox = TQueue Dynamic
type Receive = Dynamic -> StateT [Actor] (ReaderT ActorContext IO) ()

newtype Actor = Actor { receive :: Receive }

{-type Receive = Dynamic -> ReaderT ActorContext IO ()-}
{-newtype Actor = Actor { receive :: Dynamic -> ReaderT ActorContext IO () }-}

{-runActor :: Actor -> ActorContext -> Mailbox -> IO ()-}
{-runActor actor ctx mailbox = forever $ do-}
    {-msg <- atomically (readTQueue mailbox)-}
    {-actor `receive` msg `runReaderT` ctx-}

runActor :: Actor -> ActorContext -> Mailbox -> IO ()
runActor actor ctx mailbox = iterateM go []
    where go stack = do
            mail <- atomically $ readTQueue mailbox
            actor `receive` mail `execStateT` stack `runReaderT` ctx

iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f = let go = f >=> go in go

become :: MonadState [Actor] m => Receive -> m ()
become = modify . (:) . Actor

unbecome :: MonadState [s] m => m ()
unbecome = modify tail
