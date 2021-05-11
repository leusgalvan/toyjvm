module Execution(execute) where

import ClassFile as CF
import Data.Either.Combinators
import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy
import Method
import Control.Monad
import Instruction
import Frame
import Class

run :: ClassFile -> IO (Either String ())
run classFile =   
    let
        mainMethod = fromMaybe (error "No main method found") (findMainMethod classFile)
        cp = CF.constantPool classFile
        cls = fromClassFile 
    in
        return (Right ())

runMain :: ConstMethodInfo -> IO (Either String ())
runMain = undefined

executeInstruction :: Instruction -> Frame -> IO Frame
executeInstruction (IConst n) f = return (pushInt n f)
executeInstruction (IStore n) f = return (storeInt n f)
executeInstruction (GetStatic n) f = undefined
executeInstruction (ILoad n) f = undefined 
executeInstruction (InvokeVirtual n) f = undefined
executeInstruction (InvokeStatic n) f = undefined 
executeInstruction Return f = undefined 