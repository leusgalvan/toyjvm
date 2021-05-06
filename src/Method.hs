{-# LANGUAGE NamedFieldPuns #-}

module Method where

import Instruction
import ClassFile
import Data.Foldable as F
import InstructionParser
import Data.Binary.Get

data Method = Method {
    name :: String,
    instructions :: [Instruction]
} deriving (Show)

fromMethodInfo :: ConstantPool -> MethodInfo -> Method
fromMethodInfo cp MethodInfo {
    miAccessFlags,
    miNameIndex,
    miDescriptorIndex,
    miAttributes
} = 
    let
        name_ = methodName cp miNameIndex
        instructions_ = F.foldl (\xs attInfo -> getInstructions attInfo ++ xs) [] miAttributes
            where getInstructions CodeAttribute {cCode} = runGet (parseInstructions cCode) cCode
                  getInstructions _                     = []
    in
        Method { name = name_, instructions = instructions_ }