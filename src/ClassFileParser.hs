module ClassFileParser(
    parseMagicNumber,
    parseMajorVersion,
    parseMinorVersion,
    parseCpInfo
) where

import Data.Binary.Get
import Data.Binary
import Data.ByteString.Lazy
import GHC.Int
import Control.Monad

parseMagicNumber :: Get Word32
parseMagicNumber = do
    let magicNumber = 0xCAFEBABE
    firstWord <- getWord32be
    if firstWord == magicNumber then return firstWord
    else fail "Class file does not start with correct magic number"

parseMajorVersion :: Get Word16
parseMajorVersion = do
    let (minVersion, maxVersion) = (45, 52)
    version <- getWord16be
    if version >= minVersion && version <= maxVersion then return version
    else fail ("Major version must be between " ++ (show minVersion) ++
               " and " ++ (show maxVersion))

parseMinorVersion :: Get Word16
parseMinorVersion = do
    let (minVersion, maxVersion) = (0, 3)
    version <- getWord16be
    if version >= minVersion && version <= maxVersion then return version
    else fail ("Minor version must be between " ++ (show minVersion) ++
               " and " ++ (show maxVersion))
        
parseConstantPoolCount :: Get Word16
parseConstantPoolCount = getWord16be

type Tag = Word8
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

parseCpInfo :: Get CpInfo
parseCpInfo = do
    tag <- lookAhead getWord8
    case tag of
        1 -> parseCONSTANT_Utf8_info
        3 -> parseCONSTANT_Integer_info
        4 -> parseCONSTANT_Float_info
        5 -> parseCONSTANT_Long_info
        6 -> parseCONSTANT_Double_info
        7 -> parseCONSTANT_Class_info
        8 -> parseCONSTANT_String_info
        9 -> parseCONSTANT_Fieldref_info
        10 -> parseCONSTANT_Methodref_info
        11 -> parseCONSTANT_InterfaceMethodref_info
        12 -> parseCONSTANT_NameAndType_info
        15 -> parseCONSTANT_MethodHandle_info
        16 -> parseCONSTANT_MethodType_info
        18 -> parseCONSTANT_InvokeDynamic_info
        _ -> fail ("Unknown tag: " ++ (show tag))

parseCONSTANT_Utf8_info :: Get CpInfo
parseCONSTANT_Utf8_info = do
    tag <- getWord8
    length <- getWord16be
    let intLength = fromIntegral length :: Int64
    bytes <- getLazyByteString intLength
    return (CONSTANT_Utf8_info tag length bytes)

parseCONSTANT_Integer_info :: Get CpInfo
parseCONSTANT_Integer_info = do
    tag <- getWord8
    value <- getWord32be
    return (CONSTANT_Integer_info tag value)

parseCONSTANT_Float_info :: Get CpInfo
parseCONSTANT_Float_info = do
    tag <- getWord8
    value <- getWord32be
    return (CONSTANT_Float_info tag value)

parseCONSTANT_Long_info :: Get CpInfo
parseCONSTANT_Long_info = do
    tag <- getWord8
    highBytes <- getWord32be
    lowBytes <- getWord32be
    return (CONSTANT_Long_info tag highBytes lowBytes)

parseCONSTANT_Double_info :: Get CpInfo
parseCONSTANT_Double_info = do
    tag <- getWord8
    highBytes <- getWord32be
    lowBytes <- getWord32be
    return (CONSTANT_Double_info tag highBytes lowBytes)

parseCONSTANT_Class_info :: Get CpInfo
parseCONSTANT_Class_info = do
    tag <- getWord8
    nameIndex <- getWord16be
    return (CONSTANT_Class_info tag nameIndex)

parseCONSTANT_String_info :: Get CpInfo
parseCONSTANT_String_info = do
    tag <- getWord8
    stringIndex <- getWord16be
    return (CONSTANT_String_info tag stringIndex)

parseCONSTANT_Fieldref_info :: Get CpInfo
parseCONSTANT_Fieldref_info = do
    tag <- getWord8
    classIndex <- getWord16be
    nameAndTypeIndex <- getWord16be
    return (CONSTANT_Fieldref_info tag classIndex nameAndTypeIndex)

parseCONSTANT_Methodref_info :: Get CpInfo
parseCONSTANT_Methodref_info = do
    tag <- getWord8
    classIndex <- getWord16be
    nameAndTypeIndex <- getWord16be
    return (CONSTANT_Methodref_info tag classIndex nameAndTypeIndex)

parseCONSTANT_InterfaceMethodref_info :: Get CpInfo
parseCONSTANT_InterfaceMethodref_info = do
    tag <- getWord8
    classIndex <- getWord16be
    nameAndTypeIndex <- getWord16be
    return (CONSTANT_InterfaceMethodref_info tag classIndex nameAndTypeIndex)

parseCONSTANT_NameAndType_info :: Get CpInfo
parseCONSTANT_NameAndType_info = do
    tag <- getWord8
    nameIndex <- getWord16be
    descriptorIndex <- getWord16be
    return (CONSTANT_NameAndType_info tag nameIndex descriptorIndex)

parseCONSTANT_MethodHandle_info :: Get CpInfo
parseCONSTANT_MethodHandle_info = do
    tag <- getWord8
    referenceKind <- getWord8
    referenceIndex <- getWord16be
    return (CONSTANT_MethodHandle_info tag referenceKind referenceIndex)

parseCONSTANT_MethodType_info :: Get CpInfo
parseCONSTANT_MethodType_info = do
    tag <- getWord8
    descriptorIndex <- getWord16be
    return (CONSTANT_MethodType_info tag descriptorIndex)

parseCONSTANT_InvokeDynamic_info :: Get CpInfo
parseCONSTANT_InvokeDynamic_info = do
    tag <- getWord8
    bootstrapMethodAttrIndex <- getWord16be
    nameAndTypeIndex <- getWord16be
    return (CONSTANT_InvokeDynamic_info tag bootstrapMethodAttrIndex nameAndTypeIndex)

type ConstantPool = [CpInfo]
parseConstantPool :: Get ConstantPool
parseConstantPool = do
    constantPoolCount <- parseConstantPoolCount
    replicateM (fromIntegral constantPoolCount) parseCpInfo
