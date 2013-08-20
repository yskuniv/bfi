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

charToBFCommand ch = fromJust $ lookup ch charToBFCommandTable
stringToBFCommands = map charToBFCommand

data BFMemory = BFMemory { cells :: [Word8], ptr :: Word8 }

initialBFMemory = BFMemory (replicate 256 (0 :: Word8)) (0 :: Word8)
incBFMemoryPtr m = m { ptr = (succ $ ptr m) }
decBFMemoryPtr m = m { ptr = (pred $ ptr m) }
incBFMemoryDat m = setBFMemoryDat m $ succ $ getBFMemoryDat m
decBFMemoryDat m = setBFMemoryDat m $ pred $ getBFMemoryDat m
setBFMemoryDat (BFMemory cells ptr) value = BFMemory (take (fromIntegral ptr) cells ++ [value] ++ drop (fromIntegral (succ ptr)) cells) ptr
getBFMemoryDat (BFMemory cells ptr) = cells !! (fromIntegral ptr)

data BFWorld = BFWorld { commands :: [BFCommand], commandPointer :: Int, memory :: BFMemory }

createInitialBFWorld prog = BFWorld (stringToBFCommands prog) 0 initialBFMemory

searchMatchingParen :: [BFCommand] -> Int -> Int
searchMatchingParen cmds cptr = head [x | (Just x, Nothing) <- zip l (tail l)]
  where
    l :: [Maybe Int]
    l = iterate (\x -> do p <- x
                          case cmds !! p of
                            JmpFwd      -> return $ searchMatchingParen cmds p + 1
                            JmpBck      -> Nothing
                            _           -> return $ p + 1) (return $ succ cptr)

driveBFWorld :: BFWorld -> Maybe (IO BFWorld)
driveBFWorld world@(BFWorld cmds cptr mem) = if cptr >= length cmds then
                                               Nothing
                                             else
                                               Just io_world'
  where
    io_world'
      | elem cmd [IncPtr, DecPtr, IncDat, DecDat] =
        return $ world'_base { memory = (case cmd of IncPtr -> incBFMemoryPtr
                                                     DecPtr -> decBFMemoryPtr
                                                     IncDat -> incBFMemoryDat
                                                     DecDat -> decBFMemoryDat) mem }
      | cmd == OutDat = do putChar $ chr $ fromIntegral $ getBFMemoryDat mem
                           return world'_base
      | cmd == InpDat = do ch <- getChar
                           return $ world'_base { memory = setBFMemoryDat mem $ fromIntegral $ ord ch }
      | cmd == JmpFwd = runSubWorld mem
      where
        cmd = (cmds !! cptr)
        world'_base = world { commandPointer = (succ cptr) }

        runSubWorld :: BFMemory -> IO BFWorld
        runSubWorld smem = if getBFMemoryDat smem == (0 :: Word8) then
                             return world { commandPointer = cptr'
                                          , memory = smem }
                           else
                             do (BFWorld _ _ smem') <- run $ initialSubWorld { memory = smem }
                                runSubWorld smem'
          where
            cptr' = succ $ searchMatchingParen cmds cptr
            initialSubWorld = world { commands = drop (succ cptr) $ take (pred cptr') cmds
                                    , commandPointer = 0 }

run :: BFWorld -> IO BFWorld
run world = case driveBFWorld world of Nothing          -> return world
                                       (Just io_world') -> io_world' >>= run

bfi :: String -> IO ()
bfi prog = do run $ createInitialBFWorld prog
              return ()

instance Show BFCommand where
  show x = [fromJust $ lookup x $ map (\(x, y) -> (y, x)) charToBFCommandTable]

instance Show BFMemory where
  show (BFMemory cells ptr) = "[" ++ foldl (\s (x, i) -> s ++ if i == ptr then
                                                                "(" ++ show x ++ ")"
                                                              else
                                                                " " ++ show x ++ " ") "" (zip cells [0..]) ++ "]"

instance Show BFWorld where
  show (BFWorld cmds cptr mem) = "Commands:\n[" ++ foldl (\s (x, i) -> s ++ if i == cptr then
                                                                              "(" ++ show x ++ ")"
                                                                            else
                                                                              " " ++ show x ++ " ") "" (zip cmds [0..]) ++ "]\nMemory:\n" ++ show mem
