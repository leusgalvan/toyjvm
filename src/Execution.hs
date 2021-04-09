module Execution(execute) where

import ClassFile
import Data.Either.Combinators
import Text.Pretty.Simple (pPrint)

execute :: ClassFile -> IO (Either String ())
execute classFile =   
    case findMainMethod classFile of
        Just mainMethod    -> pPrint (methodCode (constantPool classFile) mainMethod) >> executeMethod mainMethod
        Nothing            -> error "No main method found"

-- >>> :t executeMethod
-- executeMethod :: MethodInfo -> IO (Either String ())
executeMethod :: MethodInfo -> IO (Either String ())
executeMethod = undefined
