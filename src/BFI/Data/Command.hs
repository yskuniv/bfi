module BFI.Data.Command where

data BFCommand
  = IncDataPtr
  | DecDataPtr
  | IncData
  | DecData
  | OutputData
  | InputData
  | CondJmpFwd
  | CondJmpBwd
  deriving (Eq)

instance Show BFCommand where
  show IncDataPtr = show ">"
  show DecDataPtr = show "<"
  show IncData = show "+"
  show DecData = show "-"
  show OutputData = show "."
  show InputData = show ","
  show CondJmpFwd = show "["
  show CondJmpBwd = show "]"
