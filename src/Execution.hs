module Execution(execute) where

import ClassFile
import Data.Either.Combinators

execute :: ClassFile -> IO (Either String ())
execute classFile =   
    case findMainMethod classFile of
        Just mainMethod    -> print mainMethod >> executeMethod mainMethod
        Nothing            -> error "No main method found"

-- >>> :t executeMethod
-- executeMethod :: MethodInfo -> IO (Either String ())
executeMethod :: MethodInfo -> IO (Either String ())
executeMethod = undefined
