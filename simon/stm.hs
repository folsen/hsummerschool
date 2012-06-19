import Control.Monad
import Control.Concurrent.STM

type CurrentSize = TVar Int
type MaxSize = Int
data BoundedChan a = BoundedChan CurrentSize MaxSize (TChan a) 

newBoundedChan   :: Int -> STM ( BoundedChan a )
newBoundedChan maxSize = do
  chan <- newTChan
  tv <- newTVar 0
  return $ BoundedChan tv maxSize chan

readBoundedChan :: BoundedChan a -> STM a
readBoundedChan (BoundedChan curr max chan) = do
  val <- readTChan chan
  c <- readTVar curr
  writeTVar curr (c-1)
  return val

writeBoundedChan :: BoundedChan a -> a -> STM ()
writeBoundedChan (BoundedChan curr max chan) val = do
  c <- readTVar curr
  when (c >= max) retry
  writeTVar curr (c+1)
  writeTChan chan val

