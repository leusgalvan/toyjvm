module ClassFileParser(parseClassFile, ClassFile) where

import ClassFile
import Data.Binary.Get
import Data.Binary
import Data.ByteString.Lazy
import GHC.Int
import Control.Monad
import Data.Bits
import qualified Data.Map.Strict as Map
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.Class
import Debug.Trace
import qualified Data.Array as A
import qualified Data.ByteString.Lazy.Char8 as Char8

debug :: (Show a, Monad m) => String -> a -> m ()
debug s a = trace (s ++ (show a)) (return ())

parseClassFile :: Get ClassFile
parseClassFile = do
    magicNumber <- parseMagicNumber
    minorVersion <- parseMinorVersion
    majorVersion <- parseMajorVersion
    constantPool <- parseConstantPool
    accessFlags <- parseAccessFlags
    thisClass <- parseThisClass
    superClass <- parseSuperClass
    interfaces <- parseInterfaces
    fields <- parseFields constantPool
    methods <- parseMethods constantPool
    attributes <- parseAttributes constantPool
    return (ClassFile {
        minorVersion = fromIntegral minorVersion, 
        majorVersion = fromIntegral majorVersion, 
        constantPool = constantPool,
        accessFlags = accessFlags, 
        thisClass = getCpInfoAtOffset constantPool (fromIntegral thisClass), 
        superClass = getCpInfoAtOffset constantPool (fromIntegral superClass),
        interfaces = fmap ((getCpInfoAtOffset constantPool) . fromIntegral) interfaces,
        fields = fields,
        methods = methods,
        attributes = attributes
    })

parseMagicNumber :: Get Word32
parseMagicNumber = do
    let magicNumber = 0xCAFEBABE
    firstWord <- getWord32be
    if firstWord == magicNumber then return firstWord
    else fail ("Class file does not start with correct magic number: " ++ (show firstWord))

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
    bytes <- getLazyByteString (fromIntegral length)
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

parseConstantPool :: Get ConstantPool
parseConstantPool = do
    constantPoolCount <- parseConstantPoolCount
    cpInfoList <- replicateM (fromIntegral (constantPoolCount-1)) parseCpInfo
    return $ buildPool (fromIntegral constantPoolCount) cpInfoList

parseAccessFlags :: Get AccessFlags
parseAccessFlags = fmap fromIntegral getWord16be

parseThisClass :: Get Word16
parseThisClass = getWord16be

parseSuperClass :: Get Word16
parseSuperClass = getWord16be

parseInterfacesCount :: Get Word16
parseInterfacesCount = getWord16be

parseInterfaces :: Get [Word16]
parseInterfaces = do
    interfacesCount <- parseInterfacesCount
    replicateM (fromIntegral interfacesCount) getWord16be

parseFieldsCount :: Get Word16
parseFieldsCount = getWord16be

parseConstantValue_attribute :: Get AttributeInfo
parseConstantValue_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    constantValueIndex <- getWord16be
    return (ConstantValue_attribute{
        cvNameIndex = nameIndex, 
        cvLength = length, 
        cvIndex = constantValueIndex
    })

parseExceptionTableEntry :: Get ExceptionTableEntry
parseExceptionTableEntry = do
    startPc <- getWord16be
    endPc <- getWord16be
    handlerPc <- getWord16be
    catchType <- getWord16be
    return (ExceptionTableEntry {
        etStartPc = startPc, 
        etEndPc = endPc, 
        etHandlerPc = handlerPc, 
        etCatchType = catchType
    })

parseCode_attribute :: ConstantPool -> Get AttributeInfo
parseCode_attribute constantPool = do
    nameIndex <- getWord16be
    length <- getWord32be
    maxStack <- getWord16be
    maxLocals <- getWord16be
    codeLength <- getWord32be
    code <- getLazyByteString (fromIntegral codeLength)
    exceptionTableLength <- getWord16be
    exceptionTable <- replicateM (fromIntegral exceptionTableLength) parseExceptionTableEntry
    attributesCount <- getWord16be
    attributes <- replicateM (fromIntegral attributesCount) (parseAttributeInfo constantPool)
    return (Code_attribute {
        cNameIndex = nameIndex, 
        cLength = length, 
        cMaxStack = maxStack, 
        cMaxLocals = maxLocals,
        cCode = code, 
        cExceptionTable = exceptionTable, 
        cAttributes = attributes
    })

parseLineNumberTableEntry :: Get LineNumberTableEntry
parseLineNumberTableEntry = do
    startPc <- getWord16be
    lineNumber <- getWord16be
    return (LineNumberTableEntry {
        ltStartPc = startPc,
        ltLineNumber = lineNumber
    })
    
parseLineNumberTable_attribute :: Get AttributeInfo
parseLineNumberTable_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    lineNumberTableLength <- getWord16be
    lineNumberTable <- replicateM (fromIntegral lineNumberTableLength) parseLineNumberTableEntry
    return (LineNumberTable_attribute {
        ltNameIndex = nameIndex, 
        ltLength = length, 
        ltLineNumberTable = lineNumberTable
    })

parseSourceFile_attribute :: Get AttributeInfo
parseSourceFile_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    index <- getWord16be
    return (SourceFile_attribute {
        sfNameIndex = nameIndex,
        sfLength = length,
        sfIndex = index
    })

parseTopVariableInfo :: Get VerificationTypeInfo
parseTopVariableInfo = fmap TopVariableInfo getWord8

parseIntegerVariableInfo :: Get VerificationTypeInfo
parseIntegerVariableInfo = fmap IntegerVariableInfo getWord8

parseFloatVariableInfo :: Get VerificationTypeInfo
parseFloatVariableInfo = fmap FloatVariableInfo getWord8

parseDoubleVariableInfo :: Get VerificationTypeInfo
parseDoubleVariableInfo = fmap DoubleVariableInfo getWord8

parseLongVariableInfo :: Get VerificationTypeInfo
parseLongVariableInfo = fmap LongVariableInfo getWord8

parseNullVariableInfo :: Get VerificationTypeInfo
parseNullVariableInfo = fmap NullVariableInfo getWord8

parseUninitializedThisVariableInfo :: Get VerificationTypeInfo
parseUninitializedThisVariableInfo = fmap UninitializedThisVariableInfo getWord8

parseObjectVariableInfo :: Get VerificationTypeInfo
parseObjectVariableInfo = do
    tag <- getWord8
    constantPoolIndex <- getWord16be
    return (ObjectVariableInfo { ovTag = tag, ovConstantPoolIndex = constantPoolIndex })

parseUninitializedVariableInfo :: Get VerificationTypeInfo
parseUninitializedVariableInfo = do
    tag <- getWord8
    offset <- getWord16be
    return (UninitializedVariableInfo { uvTag = tag, uvOffset = offset })

parseVerificationTypeInfo :: Get VerificationTypeInfo
parseVerificationTypeInfo = do
    tag <- lookAhead getWord8
    case tag of
        0 -> parseTopVariableInfo
        1 -> parseIntegerVariableInfo
        2 -> parseFloatVariableInfo
        3 -> parseDoubleVariableInfo
        4 -> parseLongVariableInfo
        5 -> parseNullVariableInfo
        6 -> parseUninitializedThisVariableInfo
        7 -> parseObjectVariableInfo
        8 -> parseUninitializedVariableInfo

parseFullFrame :: Get StackMapTableEntry
parseFullFrame = do
    frameType <- getWord8
    offsetDelta <- getWord16be
    numberOfLocals <- getWord16be
    locals <- replicateM (fromIntegral numberOfLocals) parseVerificationTypeInfo
    numberOfStackItems <- getWord16be
    stack <- replicateM (fromIntegral numberOfStackItems) parseVerificationTypeInfo
    return (FullFrame {
        ffFrameType = frameType,
        ffOffsetDelta = offsetDelta,
        ffLocals = locals,
        ffStack = stack
    })

parseAppendFrame :: Get StackMapTableEntry
parseAppendFrame = do
    frameType <- getWord8
    offsetDelta <- getWord16be
    let localsLength = frameType - 251
    locals <- replicateM (fromIntegral localsLength) parseVerificationTypeInfo
    return (AppendFrame { afFrameType = frameType, afOffsetDelta = offsetDelta, afLocals = locals })

parseSameFrameExtended :: Get StackMapTableEntry
parseSameFrameExtended = do
    frameType <- getWord8
    offsetDelta <- getWord16be
    return (SameFrameExtended { sfeFrameType = frameType, sfeOffsetDelta = offsetDelta })

parseChopFrame :: Get StackMapTableEntry
parseChopFrame = do
    frameType <- getWord8
    offsetDelta <- getWord16be
    return (ChopFrame { cfFrameType = frameType, cfOffsetDelta = offsetDelta } )

parseSameLocals1StackItemFrameExtended :: Get StackMapTableEntry
parseSameLocals1StackItemFrameExtended = do
    frameType <- getWord8
    offsetDelta <- getWord16be
    stackItem <- parseVerificationTypeInfo
    return (SameLocals1StackItemFrameExtended { 
        sleFrameType = frameType, 
        sleOffsetDelta = offsetDelta, 
        sleStack = [stackItem] 
    })

parseSameLocals1StackItemFrame :: Get StackMapTableEntry
parseSameLocals1StackItemFrame = do
    frameType <- getWord8
    stackItem <- parseVerificationTypeInfo
    return (SameLocals1StackItemFrame { slFrameType = frameType, slStack = [stackItem] })

parseSameFrame :: Get StackMapTableEntry
parseSameFrame = fmap SameFrame getWord8

parseStackMapTableEntry :: Get StackMapTableEntry
parseStackMapTableEntry = do
    frameType <- lookAhead getWord8
    if 0 <= frameType && frameType < 64 then parseSameFrame
    else if 64 <= frameType && frameType < 128 then parseSameLocals1StackItemFrame
    else if 247 == frameType then parseSameLocals1StackItemFrameExtended
    else if 248 <= frameType && frameType <= 250 then parseChopFrame
    else if 251 == frameType then parseSameFrameExtended
    else if 252 <= frameType && frameType < 255 then parseAppendFrame
    else if 255 == frameType then parseFullFrame
    else fail ("Unrecognized frame type: " ++ (show frameType))    

parseStackMapTable_attribute :: Get AttributeInfo
parseStackMapTable_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    numberOfEntries <- getWord16be
    stackMapTable <- replicateM (fromIntegral numberOfEntries) parseStackMapTableEntry
    return (StackMapTable_attribute {
        smNameIndex = nameIndex,
        smLength = length,
        smTable = stackMapTable
    })

parseExceptions_attribute :: Get AttributeInfo
parseExceptions_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    numberOfExceptions <- getWord16be
    indexTable <- replicateM (fromIntegral numberOfExceptions) getWord16be
    return (Exceptions_attribute {
        eNameIndex = nameIndex,
        eLength = length,
        eIndexTable = indexTable
    })

parseInnerClass :: Get InnerClass
parseInnerClass = do
    innerClassInfoIndex <- getWord16be
    outerClassInfoIndex <- getWord16be
    innerNameIndex <- getWord16be
    innerClassAccessFlags <- parseAccessFlags
    return (InnerClass {
        icInnerClassInfoIndex = innerClassInfoIndex,
        icOuterClassInfoIndex = outerClassInfoIndex,
        icInnerNameIndex = innerNameIndex,
        icInnerClassAccessFlags = innerClassAccessFlags
    })

parseInnerClasses_attribute :: Get AttributeInfo
parseInnerClasses_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    numberOfClasses <- getWord16be
    classes <- replicateM (fromIntegral numberOfClasses) parseInnerClass
    return (InnerClasses_attribute {
        icNameIndex = nameIndex,
        icLength = length,
        icClasses = classes
    })

parseEnclosingMethod_attribute :: Get AttributeInfo
parseEnclosingMethod_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    classIndex <- getWord16be
    methodIndex <- getWord16be
    return (EnclosingMethod_attribute {
        emNameIndex = nameIndex,
        emLength = length,
        emClassIndex = classIndex,
        emMethodIndex = methodIndex
    })

parseSignature_attribute :: Get AttributeInfo
parseSignature_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    signatureIndex <- getWord16be
    return (Signature_attribute {
        sNameIndex = nameIndex,
        sLength = length,
        sSignatureIndex = signatureIndex
    })

parseSynthetic_attribute :: Get AttributeInfo
parseSynthetic_attribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    return (Synthetic_attribute { synNameIndex = nameIndex, synLength = length })

parseAttributeInfo :: ConstantPool -> Get AttributeInfo
parseAttributeInfo constantPool = do
    nameIndex <- lookAhead getWord16be
    let attributeCpInfo = getCpInfoAtOffset constantPool (fromIntegral nameIndex)
    let attributeName = case attributeCpInfo of
                            CONSTANT_Utf8_info _ _ byteString -> Char8.unpack byteString
                            x -> fail ("Expected entry of type CONSTANT_Utf8_info but got: " ++ (show x))
    case attributeName of
        "ConstantValue" -> parseConstantValue_attribute
        "Code" -> parseCode_attribute constantPool
        "LineNumberTable" -> parseLineNumberTable_attribute
        "SourceFile" -> parseSourceFile_attribute
        "StackMapTable" -> parseStackMapTable_attribute
        "Exceptions" -> parseExceptions_attribute
        "InnerClasses" -> parseInnerClasses_attribute
        "EnclosingMethod" -> parseEnclosingMethod_attribute
        "Signature" -> parseSignature_attribute
        "Synthetic" -> parseSynthetic_attribute
        s -> fail ("Unrecognized attribute name: " ++ (show s))

parseAttributes :: ConstantPool -> Get [AttributeInfo]
parseAttributes constantPool = do
    attributesCount <- getWord16be
    replicateM (fromIntegral attributesCount) (parseAttributeInfo constantPool)

parseFieldInfo :: ConstantPool -> Get FieldInfo
parseFieldInfo constantPool = do
    accessFlags <- parseAccessFlags
    nameIndex <- getWord16be
    descriptorIndex <- getWord16be
    attributesCount <- getWord16be
    attributeInfo <- replicateM (fromIntegral attributesCount) (parseAttributeInfo constantPool)
    return (FieldInfo {
        fiAccessFlags = accessFlags, 
        fiNameIndex = nameIndex, 
        fiDescriptorIndex = descriptorIndex, 
        fiAttributesCount = attributesCount, 
        fiAttributeInfo = attributeInfo
    })

parseFields :: ConstantPool -> Get [FieldInfo]
parseFields constantPool = do
    fieldsCount <- parseFieldsCount
    replicateM (fromIntegral fieldsCount) (parseFieldInfo constantPool)

parseMethodsCount :: Get Word16
parseMethodsCount = getWord16be

parseMethodInfo :: ConstantPool -> Get MethodInfo
parseMethodInfo constantPool = do
    accessFlags <- parseAccessFlags
    nameIndex <- getWord16be
    descriptorIndex <- getWord16be
    attributesCount <- getWord16be
    attributes <- replicateM (fromIntegral attributesCount) (parseAttributeInfo constantPool)
    return (MethodInfo {
        miAccessFlags = accessFlags, 
        miNameIndex = nameIndex, 
        miDescriptorIndex = descriptorIndex, 
        miAttributes = attributes
    })

parseMethods :: ConstantPool -> Get [MethodInfo]
parseMethods constantPool = do 
  methodsCount <- parseMethodsCount
  replicateM (fromIntegral methodsCount) (parseMethodInfo constantPool)

