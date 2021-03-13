module BFI.CommandRunner where

import BFI.BFMemory
import BFI.Data.Command
import Control.Monad.IO.Class (MonadIO (liftIO))

type BFIState = BFMemoryStateT IO

runBFIState :: BFIState () -> IO [BFMemoryData]
runBFIState = runBFMemoryT

runCommands :: [BFCommand] -> BFIState ()
runCommands [] = return ()
runCommands (c : cs) = do
  case c of
    IncDataPtr -> do
      runIncDataPtr
      runCommands cs
    DecDataPtr -> do
      runDecDataPtr
      runCommands cs
    IncData -> do
      runIncData
      runCommands cs
    DecData -> do
      runDecData
      runCommands cs
    OutputData -> do
      runOutputData
      runCommands cs

runIncDataPtr :: BFIState ()
runIncDataPtr = do
  incBFMemoryPtr

runDecDataPtr :: BFIState ()
runDecDataPtr = do
  decBFMemoryPtr

runIncData :: BFIState ()
runIncData = do
  incBFMemoryData

runDecData :: BFIState ()
runDecData = do
  decBFMemoryData

runOutputData :: BFIState ()
runOutputData = do
  v <- getBFMemoryData
  liftIO $ print v
