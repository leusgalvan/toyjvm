module InstructionParser where

import Data.ByteString.Lazy
import Instruction
import Data.Binary.Get

import Control.Monad.Loops

parseInstructions :: ByteString -> Get [Instruction]
parseInstructions bytes = untilM parseInstruction isEmpty

parseInstruction :: Get Instruction 
parseInstruction = do
    opCode <- getWord8
    case opCode of
      8   -> parseIConst 5
      60  -> parseIStore 1
      178 -> parseGetStatic
      27  -> parseILoad 1
      182 -> parseInvokeVirtual
      177 -> parseReturn
      _   -> fail ("Unrecognized opcode: " ++ show opCode)

parseIConst :: Int -> Get Instruction
parseIConst = return . IConst

parseIStore :: Int -> Get Instruction
parseIStore = return . IStore

parseGetStatic :: Get Instruction 
parseGetStatic = fmap (GetStatic . fromIntegral) getWord16be 

parseILoad :: Int -> Get Instruction 
parseILoad = return . ILoad

parseInvokeVirtual :: Get Instruction 
parseInvokeVirtual = fmap (InvokeVirtual . fromIntegral) getWord16be

parseReturn :: Get Instruction 
parseReturn = return Return 