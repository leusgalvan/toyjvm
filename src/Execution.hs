module Execution(execute) where

import ClassFile

execute :: IO ClassFile
execute = undefined

findMainMethod :: ClassFile -> Either String MethodInfo
findMainMethod = undefined