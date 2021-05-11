{-# LANGUAGE NamedFieldPuns #-}

module Class where

import Method
import Field
import ClassFile as CF
import ClassLoader as CL
import ClassFileParser as P

data Class = Class {
    simpleName :: String,
    package :: String,
    methods :: [Method],
    fields :: [Field],
    constantPool :: ConstantPool
}

fromClassFile :: ClassFile -> Class
fromClassFile cf@ClassFile {
    minorVersion,
    majorVersion,
    CF.constantPool,
    accessFlags,
    thisClass,
    superClass,
    interfaces,
    CF.fields,
    CF.methods,
    attributes
} = undefined
