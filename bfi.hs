import Data.Binary
import Data.Maybe
import Data.Char

charToBFCommandTable = [('>', IncPtr),
                        ('<', DecPtr),
                        ('+', IncDat),
                        ('-', DecDat),
                        ('.', OutDat),
                        (',', InpDat),
                        ('[', JmpFwd),
                        (']', JmpBck)]

data BFCommand = IncPtr | DecPtr | IncDat | DecDat | OutDat | InpDat | JmpFwd | JmpBck deriving Eq

instance Show BFCommand where
  show x = [fromJust $ lookup x $ map (\(x, y) -> (y, x)) charToBFCommandTable]

charToBFCommand :: Char -> BFCommand
charToBFCommand ch = fromJust $ lookup ch charToBFCommandTable

stringToBFCommands = map charToBFCommand

data BFMemory = BFMemory { cells :: [Word8], ptr :: Word8 }

incBFMemoryPtr m = m { ptr = (succ $ ptr m) }
decBFMemoryPtr m = m { ptr = (pred $ ptr m) }
incBFMemoryDat m = setBFMemoryDat m $ succ $ getBFMemoryDat m
decBFMemoryDat m = setBFMemoryDat m $ pred $ getBFMemoryDat m
setBFMemoryDat (BFMemory cells ptr) value = BFMemory (take (fromIntegral ptr) cells ++ [value] ++ drop (fromIntegral (succ ptr)) cells) ptr
getBFMemoryDat (BFMemory cells ptr) = cells !! (fromIntegral ptr)

data BFWorld = BFWorld { commands :: [BFCommand], commandPointer :: Int, memory :: BFMemory }

instance Show BFWorld where
  show (BFWorld cmds cptr (BFMemory cells ptr)) = "cmds = " ++ show cmds ++ ", cptr = " ++ show cptr ++ ", mcells = " ++ show cells ++ ", mptr = " ++ show ptr

createInitialBFWorld prog = BFWorld (stringToBFCommands prog) 0 $ BFMemory (replicate 256 (0 :: Word8)) (0 :: Word8)

searchMatchingParen :: [BFCommand] -> Int -> Int
searchMatchingParen cmds cptr = head [x | (Just x, Nothing) <- zip l (tail l)]
  where
    l :: [Maybe Int]
    l = iterate (\x -> do p <- x
                          case cmds !! p of
                            JmpFwd      -> return $ searchMatchingParen cmds p + 1
                            JmpBck      -> Nothing
                            _           -> return $ p + 1) (return $ succ cptr)

driveBFWorld :: BFWorld -> IO (Maybe BFWorld)
driveBFWorld world@(BFWorld cmds cptr mem) = if cptr >= length cmds then
                                               return Nothing
                                             else
                                               io_mb_world'
  where
    io_mb_world'
      | elem cmd [IncPtr, DecPtr, IncDat, DecDat] =
        return $ Just $ world'_base { memory = (case cmd of IncPtr -> incBFMemoryPtr
                                                            DecPtr -> decBFMemoryPtr
                                                            IncDat -> incBFMemoryDat
                                                            DecDat -> decBFMemoryDat) mem }
      | cmd == OutDat = do putChar $ chr $ fromIntegral $ getBFMemoryDat mem
                           return $ Just world'_base
      | cmd == InpDat = do ch <- getChar
                           return $ Just $ world'_base { memory = setBFMemoryDat mem $ fromIntegral $ ord ch }
      | cmd == JmpFwd = do (BFWorld _ _ mem') <- runSubWorld $ world { commands = drop (succ cptr) $ take (pred cptr') cmds
                                                                     , commandPointer = 0 }
                           return $ Just $ world'_base { memory = mem' }
      where
        cmd = (cmds !! cptr)
        cptr' = succ $ case cmd of JmpFwd -> searchMatchingParen cmds cptr
                                   _      -> cptr
        world'_base = world { commandPointer = cptr' }

        runSubWorld sw@(BFWorld _ _ smem) = if getBFMemoryDat smem == (0 :: Word8) then
                                              return sw
                                            else
                                              do (BFWorld _ _ smem') <- run sw
                                                 runSubWorld $ sw { memory = smem' }

run :: BFWorld -> IO BFWorld
run world = do mb_world' <- driveBFWorld world
               case mb_world' of Nothing        -> return world
                                 (Just world')  -> run world'

bfi :: String -> IO ()
bfi prog = do run $ createInitialBFWorld prog
              return ()
