module BFI.BFMemory
  ( BFMemoryData,
    BFMemoryStateT,
    BFMemoryState,
    runBFMemoryT,
    runBFMemory,
    getBFMemoryData,
    setBFMemoryData,
    incBFMemoryData,
    decBFMemoryData,
    incBFMemoryPtr,
    decBFMemoryPtr,
  )
where

import BFI.Utils.DataArray
import Control.Monad.Identity
import Control.Monad.State
import Data.Binary (Word8)

type BFMemoryData = Word8

type BFMemoryStateT m = StateT Int (DataArrayStateT BFMemoryData m)

type BFMemoryState = BFMemoryStateT Identity

runBFMemoryT :: Monad m => BFMemoryStateT m a -> m [BFMemoryData]
runBFMemoryT s = runDataArrayT (execStateT s 0) 0

runBFMemory :: BFMemoryState a -> [BFMemoryData]
runBFMemory s = runIdentity $ runBFMemoryT s

getBFMemoryData :: Monad m => BFMemoryStateT m BFMemoryData
getBFMemoryData = do
  ptr <- get
  lift $ readDataArray ptr

setBFMemoryData :: Monad m => BFMemoryData -> BFMemoryStateT m ()
setBFMemoryData v = do
  ptr <- get
  lift $ writeDataArray ptr v

incBFMemoryData :: Monad m => BFMemoryStateT m ()
incBFMemoryData = do
  ptr <- get
  lift $ do
    v <- readDataArray ptr
    writeDataArray ptr (v + 1)

decBFMemoryData :: Monad m => BFMemoryStateT m ()
decBFMemoryData = do
  ptr <- get
  lift $ do
    v <- readDataArray ptr
    writeDataArray ptr (v - 1)

incBFMemoryPtr :: Monad m => BFMemoryStateT m ()
incBFMemoryPtr = modify succ

decBFMemoryPtr :: Monad m => BFMemoryStateT m ()
decBFMemoryPtr = modify pred
