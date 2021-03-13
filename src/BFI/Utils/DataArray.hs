module BFI.Utils.DataArray
  ( DataArrayStateT,
    DataArrayState,
    runDataArrayT,
    runDataArray,
    readDataArray,
    writeDataArray,
  )
where

import Control.Monad.Identity
import Control.Monad.State

type DataArrayStateT e = StateT [e]

type DataArrayState e = DataArrayStateT e Identity

runDataArrayT :: Monad m => DataArrayStateT e m a -> e -> m [e]
runDataArrayT s init = execStateT s $ genArray init

runDataArray :: DataArrayState e a -> e -> [e]
runDataArray s init = runIdentity $ runDataArrayT s init

readDataArray :: Monad m => Int -> DataArrayStateT e m e
readDataArray i = do
  a <- get
  return $ getElemAt a i

writeDataArray :: Monad m => Int -> e -> DataArrayStateT e m ()
writeDataArray i v = do
  a <- get
  put $ updateElemAt a i v

getElemAt :: [e] -> Int -> e
getElemAt a i = a !! i

updateElemAt :: [e] -> Int -> e -> [e]
updateElemAt a i v = take i a ++ [v] ++ drop (i + 1) a

genArray :: e -> [e]
genArray = repeat
