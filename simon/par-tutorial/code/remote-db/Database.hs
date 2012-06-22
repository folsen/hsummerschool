{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Database (
       Database,
       Key, Value,
       createDB,
       getDB, set,
       rcdata,
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Remote
import Data.DeriveTH
import Data.Binary
import Data.Typeable
import Control.Monad (forM)

-- import Control.Concurrent.STM
-- import Control.Monad.IO.Class

type Key   = String
type Value = String

type Database = ProcessId

data Query = Get Key ProcessId
           | Set Key Value
  deriving Typeable

$( derive makeBinary ''Query )
  
--type DB = TVar (Map Key Value)
doDbServer :: Map Key Value -> ProcessM ()
doDbServer map = do
  query <- expect
  map' <- case query of
    (Get key from) -> do
      let ret = Map.lookup key map
      send from ret
      return map

    (Set key val) -> return (Map.insert key val map)  
  -- mypid <- getSelfPid
  doDbServer map'  


dbServer :: ProcessM ()
dbServer = doDbServer Map.empty

$( remotable ['dbServer])

createWorker :: NodeId -> ProcessM Database
createWorker node = do
  process <- spawn node dbServer__closure
  return process

doMainDB workers = do
  newQuery <- expect --client
  case newQuery of 
    (Get k from)  -> do
      let pid = hash k workers

      myPid <- getSelfPid
      send pid (Get k myPid) --db 
      v <- expect -- db

      send from v -- client

    (Set k v) -> do
        let pid = hash k workers
        send pid k
  doMainDB workers


workerMax :: Int
workerMax = 5
hash :: Key -> [ProcessId] -> ProcessId
hash []     _  = undefined
hash (x:xs) ws = Data.Char.digitToInt x `mod` workerMax


dbMainDBclj :: ProcessM ()
doMainDBclj = do
  node <- getSelfNode
  workerPids <- forM [1..workerMax] (createWorker node)
  return (doMainDB workerPids)

$( remotable ['doMainDBclj])

createDB :: Int -> Process Database
createDB n = do
  node <- getSelfNode
  mainDB <- spawn node doMainDBclj__closure
  return mainDB

set :: Database -> Key -> Value -> ProcessM ()
set db k v = do
  myPid <- getSelfPid
  send db (Set k v)  
--error "not implemented!" -- exercise 5.1

getDB :: Database -> Key -> ProcessM (Maybe Value)
getDB db k = do
  myPid <- getSelfPid
  send db (Get k myPid)
  expect



rcdata = [Database.__remoteCallMetaData] -- exercise 5.1: you will need to add any necessary
            -- __remoteCallMetadata to this list, e.g.
            -- [Database.__remoteCallMetadata]
