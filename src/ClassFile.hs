module ClassFile(
    Version,
    ClassFile(..),
    Tag,
    showTag,
    CpInfo(..),
    ConstantPool, 
    getCpInfoAtOffset,
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
    MethodInfo(..)
) where

import Data.Word
import Data.ByteString.Lazy
import qualified Data.Array as A

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

data CpInfo = CONSTANT_Utf8_info Tag Word16 ByteString |
    CONSTANT_Integer_info Tag Word32 |
    CONSTANT_Float_info Tag Word32 |
    CONSTANT_Long_info Tag Word32 Word32 |
    CONSTANT_Double_info Tag Word32 Word32 |
    CONSTANT_Class_info Tag Word16 |
    CONSTANT_String_info Tag Word16 |
    CONSTANT_Fieldref_info Tag Word16 Word16 |
    CONSTANT_Methodref_info Tag Word16 Word16 |
    CONSTANT_InterfaceMethodref_info Tag Word16 Word16 |
    CONSTANT_NameAndType_info Tag Word16 Word16 |
    CONSTANT_MethodHandle_info Tag Word8 Word16 |
    CONSTANT_MethodType_info Tag Word16 |
    CONSTANT_InvokeDynamic_info Tag Word16 Word16
    deriving Show

type ConstantPool = A.Array Int CpInfo
getCpInfoAtOffset :: ConstantPool -> Int -> CpInfo
getCpInfoAtOffset = (A.!) 

buildPool :: Int -> [CpInfo] -> ConstantPool
buildPool n xs = A.listArray (1, n-1) xs

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
    ConstantValue_attribute {
        cvNameIndex :: Word16, 
        cvLength :: Word32, 
        cvIndex :: Word16
    } |
    Code_attribute {
        cNameIndex :: Word16, 
        cLength :: Word32, 
        cMaxStack :: Word16, 
        cMaxLocals :: Word16,
        cCode :: ByteString, 
        cExceptionTable :: ExceptionTable, 
        cAttributes :: [AttributeInfo]
    } |
    LineNumberTable_attribute {
        ltNameIndex :: Word16,
        ltLength :: Word32,
        ltLineNumberTable :: LineNumberTable
    } |
    SourceFile_attribute {
        sfNameIndex :: Word16,
        sfLength :: Word32,
        sfIndex :: Word16
    } |
    StackMapTable_attribute {
        smNameIndex :: Word16,
        smLength :: Word32,
        smTable :: StackMapTable
    } |
    Exceptions_attribute {
        eNameIndex :: Word16,
        eLength :: Word32,
        eIndexTable :: [Word16]
    } |
    InnerClasses_attribute {
        icNameIndex :: Word16,
        icLength :: Word32,
        icClasses :: [InnerClass]
    } |
    EnclosingMethod_attribute {
        emNameIndex :: Word16,
        emLength :: Word32,
        emClassIndex :: Word16,
        emMethodIndex :: Word16
    } |
    Signature_attribute {
        sNameIndex :: Word16,
        sLength :: Word32,
        sSignatureIndex :: Word16
    } |
    Synthetic_attribute {
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