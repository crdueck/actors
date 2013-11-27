{-# LANGUAGE RankNTypes #-}

module ActorRef where

import ActorPath

import Data.Typeable
import Data.Unique

data ActorRef = ActorRef
    { (!)  :: forall a. Typeable a => a -> IO ()
    , path :: ActorPath
    , uuid :: Unique
    }

instance Show ActorRef where
    show = show . path

instance Eq ActorRef where
    a1 == a2 = uuid a1 == uuid a2

instance Ord ActorRef where
    a1 `compare` a2 = uuid a1 `compare` uuid a2
