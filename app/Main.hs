module Main where

import BFI.CommandRunner
import BFI.Data.Command
import BFI.ProgramLoader
-- import Control.Monad.Except (MonadError (catchError))
import Data.Semigroup ((<>))
import Options.Applicative

main :: IO ()
main = runBFFile =<< execParser opts
  where
    opts =
      info
        ( argument str (metavar "FILE")
            <**> helper
        )
        ( fullDesc
            <> progDesc "Run FILE"
            <> header "haskell-bfi - a BFI implemented in haskell"
        )

runBFFile :: FilePath -> IO ()
runBFFile path = runProgram =<< readFile path

runProgram :: String -> IO ()
runProgram prog = do
  runBFIState $ runCommands $ loadProgram prog
  return ()
