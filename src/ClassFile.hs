{-# LANGUAGE NamedFieldPuns #-}

module ClassFile(
    findMainMethod,
    Version,
    ClassFile(..),
    Tag,
    showTag,
    CpInfo(..),
    ConstantPool, 
    getCpInfoAtIndex,
    buildPool,
    AccessFlags,
    ExceptionTableEntry(..),
    ExceptionTable,
    LineNumberTableEntry(..),
    LineNumberTable,
    VerificationTypeInfo(..),
    StackMapTableEntry(..),
    StackMapTable,
    InnerClass(..),
    AttributeInfo(..),
    FieldInfo(..),
    MethodInfo(..),
    methodCode,
    methodName
) where

import Data.Word
import Data.ByteString.Lazy
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Maybe as M

type Version = Int
data ClassFile = ClassFile {
    minorVersion :: Version,
    majorVersion :: Version,
    constantPool :: ConstantPool,
    accessFlags :: AccessFlags,
    thisClass :: CpInfo,
    superClass :: CpInfo,
    interfaces :: [CpInfo],
    fields :: [FieldInfo],
    methods :: [MethodInfo],
    attributes :: [AttributeInfo]
} deriving (Show)

findMainMethod :: ClassFile -> Maybe MethodInfo
findMainMethod ClassFile {constantPool, methods} = 
    L.find (isMain constantPool) methods

type Tag = Word8

showTag :: Tag -> String
showTag t = case t of
                0 -> "Empty"
                1 -> "Utf8"
                3 -> "Integer"
                4 -> "Float"
                5 -> "Long"
                6 -> "Double"
                7 -> "Class"
                8 -> "String"
                9 -> "Fieldref"
                10 -> "Methodref"
                11 -> "InterfaceMethodRef"
                12 -> "NameAndType"

data CpInfo = ConstantUtf8Info Tag Word16 ByteString |
    ConstantIntegerInfo Tag Word32 |
    ConstantFloatInfo Tag Word32 |
    ConstantLongInfo Tag Word32 Word32 |
    ConstantDoubleInfo Tag Word32 Word32 |
    ConstantClassInfo Tag Word16 |
    ConstantStringInfo Tag Word16 |
    ConstantFieldrefInfo Tag Word16 Word16 |
    ConstantMethodrefInfo Tag Word16 Word16 |
    ConstantInterfaceMethodrefInfo Tag Word16 Word16 |
    ConstantNameAndTypeInfo Tag Word16 Word16 |
    ConstantMethodHandleInfo Tag Word8 Word16 |
    ConstantMethodTypeInfo Tag Word16 |
    ConstantInvokeDynamicInfo Tag Word16 Word16
    deriving Show

type ConstantPool = A.Array Int CpInfo

getCpInfoAtIndex :: ConstantPool -> Int -> CpInfo
getCpInfoAtIndex = (A.!) 

utf8InfoAsString :: CpInfo -> String
utf8InfoAsString (ConstantUtf8Info _ _ byteString) = L.filter (/='"') (show byteString)

buildPool :: Int -> [CpInfo] -> ConstantPool
buildPool n = A.listArray (1, n-1)

type AccessFlags = Int

data ExceptionTableEntry = ExceptionTableEntry {
    etStartPc :: Word16, 
    etEndPc :: Word16, 
    etHandlerPc :: Word16, 
    etCatchType :: Word16
} deriving (Show)

type ExceptionTable = [ExceptionTableEntry]

data LineNumberTableEntry = LineNumberTableEntry {
    ltStartPc :: Word16,
    ltLineNumber :: Word16
} deriving (Show)

type LineNumberTable = [LineNumberTableEntry]

data VerificationTypeInfo = TopVariableInfo { tvTag :: Word8 } |
    IntegerVariableInfo { ivTag :: Word8 } |
    FloatVariableInfo { fvTag :: Word8 } |
    LongVariableInfo { lv :: Word8 } |
    DoubleVariableInfo { dvTag :: Word8 } |
    NullVariableInfo { nvTag :: Word8 } |
    UninitializedThisVariableInfo { utvTag :: Word8 } |
    ObjectVariableInfo { ovTag :: Word8, ovConstantPoolIndex :: Word16 } |
    UninitializedVariableInfo { uvTag :: Word8, uvOffset :: Word16 }
    deriving (Show)

data StackMapTableEntry = SameFrame { sfFrameType :: Word8 } |
    SameLocals1StackItemFrame { slFrameType :: Word8, slStack :: [VerificationTypeInfo] } |
    SameLocals1StackItemFrameExtended { sleFrameType :: Word8, sleOffsetDelta :: Word16, 
                                        sleStack :: [VerificationTypeInfo] } |
    ChopFrame { cfFrameType :: Word8, cfOffsetDelta :: Word16 } |
    SameFrameExtended { sfeFrameType :: Word8, sfeOffsetDelta :: Word16 } |
    AppendFrame { afFrameType :: Word8, afOffsetDelta :: Word16, afLocals :: [VerificationTypeInfo] } |
    FullFrame { ffFrameType :: Word8, ffOffsetDelta :: Word16, ffLocals :: [VerificationTypeInfo], 
                ffStack :: [VerificationTypeInfo] }
    deriving (Show)

type StackMapTable = [StackMapTableEntry]

data InnerClass = InnerClass {
    icInnerClassInfoIndex :: Word16,
    icOuterClassInfoIndex :: Word16,
    icInnerNameIndex :: Word16,
    icInnerClassAccessFlags :: AccessFlags
} deriving (Show)


data AttributeInfo = 
    ConstantValueAttribute {
        cvNameIndex :: Word16, 
        cvLength :: Word32, 
        cvIndex :: Word16
    } |
    CodeAttribute {
        cNameIndex :: Word16, 
        cLength :: Word32, 
        cMaxStack :: Word16, 
        cMaxLocals :: Word16,
        cCode :: ByteString, 
        cExceptionTable :: ExceptionTable, 
        cAttributes :: [AttributeInfo]
    } |
    LineNumberTableAttribute {
        ltNameIndex :: Word16,
        ltLength :: Word32,
        ltLineNumberTable :: LineNumberTable
    } |
    SourceFileAttribute {
        sfNameIndex :: Word16,
        sfLength :: Word32,
        sfIndex :: Word16
    } |
    StackMapTableAttribute {
        smNameIndex :: Word16,
        smLength :: Word32,
        smTable :: StackMapTable
    } |
    ExceptionsAttribute {
        eNameIndex :: Word16,
        eLength :: Word32,
        eIndexTable :: [Word16]
    } |
    InnerClassesAttribute {
        icNameIndex :: Word16,
        icLength :: Word32,
        icClasses :: [InnerClass]
    } |
    EnclosingMethodAttribute {
        emNameIndex :: Word16,
        emLength :: Word32,
        emClassIndex :: Word16,
        emMethodIndex :: Word16
    } |
    SignatureAttribute {
        sNameIndex :: Word16,
        sLength :: Word32,
        sSignatureIndex :: Word16
    } |
    SyntheticAttribute {
        synNameIndex :: Word16,
        synLength :: Word32
    }
    deriving (Show)

data FieldInfo = FieldInfo {
    fiAccessFlags :: AccessFlags,
    fiNameIndex :: Word16,
    fiDescriptorIndex :: Word16,
    fiAttributesCount :: Word16,
    fiAttributeInfo :: [AttributeInfo]
} deriving (Show)

data MethodInfo = MethodInfo {
    miAccessFlags :: AccessFlags,
    miNameIndex :: Word16,
    miDescriptorIndex :: Word16,
    miAttributes :: [AttributeInfo]
} deriving (Show)

methodCode :: ConstantPool -> MethodInfo -> AttributeInfo
methodCode cp mi = M.fromJust (L.find isCodeAttribute (miAttributes mi))
    where isCodeAttribute CodeAttribute{} = True
          isCodeAttribute _ = False

methodName :: ConstantPool -> Word16 -> String
methodName cp = utf8InfoAsString . getCpInfoAtIndex cp . fromIntegral

isMain :: ConstantPool -> MethodInfo -> Bool
isMain cp mi = methodName cp (miNameIndex mi) == "main"