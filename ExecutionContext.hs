module ExecutionContext where

import Control.Concurrent
import Control.Monad

newtype ExecutionContext = ExecutionContext { offer :: IO () -> IO () }

globalExecutionContext :: ExecutionContext
globalExecutionContext = ExecutionContext $ void . forkIO
