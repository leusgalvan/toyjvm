module Execution(execute) where

import ClassFile
import Data.Either.Combinators
import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy
import Method

execute :: ClassFile -> IO (Either String ())
execute classFile =   
    case findMainMethod classFile of
        Just mainMethod    -> pPrint (unpack (cCode (methodCode (constantPool classFile) mainMethod))) >> executeMethod (constantPool classFile) mainMethod
        Nothing            -> error "No main method found"

executeMethod :: ConstantPool -> MethodInfo -> IO (Either String ())
executeMethod cp mi = print (fromMethodInfo cp mi) >> return (Right ())
