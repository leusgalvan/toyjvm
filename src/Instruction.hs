module Instruction where

data Instruction = IConst Int |
    IStore Int |
    GetStatic Int |
    ILoad Int |
    InvokeVirtual Int |
    Return 
    deriving (Show)