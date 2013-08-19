import Data.Binary
import Data.Maybe

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

initialBFWorld prog = BFWorld (stringToBFCommands prog) 0 $ BFMemory (replicate 256 (0 :: Word8)) (0 :: Word8)

searchMatchingParen :: [BFCommand] -> Int -> Int
searchMatchingParen cmds cptr = head [x | (Just x, Nothing) <- zip l (tail l)]
  where
    l :: [Maybe Int]
    l = iterate (\x -> do p <- x
                          case cmds !! p of
                            JmpFwd      -> return $ searchMatchingParen cmds p + 1
                            JmpBck      -> Nothing
                            _           -> return $ p + 1) (return $ succ cptr)

driveBFWorld :: BFWorld -> Maybe BFWorld
driveBFWorld world@(BFWorld cmds cptr mem) = if cptr >= length cmds then
                                               Nothing
                                             else
                                               return world'
  where
    world' | cmd == IncPtr || cmd == DecPtr || cmd == IncDat || cmd == DecDat =
      world { commandPointer = (succ cptr), memory = (case cmd of IncPtr -> incBFMemoryPtr
                                                                  DecPtr -> decBFMemoryPtr
                                                                  IncDat -> incBFMemoryDat
                                                                  DecDat -> decBFMemoryDat) mem }
           | cmd == JmpFwd = if getBFMemoryDat mem == (0 :: Word8) then
                               world { commandPointer = (succ p) }
                             else
                               runSubWorld $ world { commands = (drop (succ cptr) $ take p cmds), commandPointer = 0 }
      where
        cmd = (cmds !! cptr)
        p = searchMatchingParen cmds cptr
        runSubWorld sw = if getBFMemoryDat mem' == (0 :: Word8) then
                           world { commandPointer = (succ p), memory = mem' }
                         else
                           runSubWorld $ sw { memory = mem' }
          where
            (BFWorld _ _ mem') = run sw

run :: BFWorld -> BFWorld
run world = if isNothing world' then
              world
            else
              run $ fromJust world'
  where
    world' = driveBFWorld world

bfi :: String -> BFWorld
bfi prog = run $ initialBFWorld prog
