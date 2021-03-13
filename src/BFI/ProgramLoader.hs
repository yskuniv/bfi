module BFI.ProgramLoader
  ( loadProgram,
  )
where

import BFI.Data.Command

loadProgram :: String -> [BFCommand]
loadProgram =
  foldr
    ( \c ->
        case toBFCommand c of
          Just cmd -> (cmd :)
          Nothing -> id
    )
    []

toBFCommand :: Char -> Maybe BFCommand
toBFCommand '>' = return IncDataPtr
toBFCommand '<' = return DecDataPtr
toBFCommand '+' = return IncData
toBFCommand '-' = return DecData
toBFCommand '.' = return OutputData
toBFCommand ',' = return InputData
toBFCommand '[' = return CondJmpFwd
toBFCommand ']' = return CondJmpBwd
toBFCommand _ = Nothing
