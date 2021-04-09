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
debug s a = trace (s ++ show a) (return ())

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
        thisClass = getCpInfoAtIndex constantPool (fromIntegral thisClass), 
        superClass = getCpInfoAtIndex constantPool (fromIntegral superClass),
        interfaces = fmap (getCpInfoAtIndex constantPool . fromIntegral) interfaces,
        fields = fields,
        methods = methods,
        attributes = attributes
    })

parseMagicNumber :: Get Word32
parseMagicNumber = do
    let magicNumber = 0xCAFEBABE
    firstWord <- getWord32be
    if firstWord == magicNumber then return firstWord
    else fail ("Class file does not start with correct magic number: " ++ show firstWord)

parseMajorVersion :: Get Word16
parseMajorVersion = do
    let (minVersion, maxVersion) = (45, 52)
    version <- getWord16be
    if version >= minVersion && version <= maxVersion then return version
    else fail ("Major version must be between " ++ show minVersion ++
               " and " ++ show maxVersion)

parseMinorVersion :: Get Word16
parseMinorVersion = do
    let (minVersion, maxVersion) = (0, 3)
    version <- getWord16be
    if version >= minVersion && version <= maxVersion then return version
    else fail ("Minor version must be between " ++ show minVersion ++
               " and " ++ show maxVersion)
        
parseConstantPoolCount :: Get Word16
parseConstantPoolCount = getWord16be

parseCpInfo :: Get CpInfo
parseCpInfo = do
    tag <- lookAhead getWord8
    case tag of
        1 -> parseConstantUtf8Info
        3 -> parseConstantIntegerInfo
        4 -> parseConstantFloatInfo
        5 -> parseConstantLongInfo
        6 -> parseConstantDoubleInfo
        7 -> parseConstantClassInfo
        8 -> parseConstantStringInfo
        9 -> parseConstantFieldrefInfo
        10 -> parseConstantMethodrefInfo
        11 -> parseConstantInterfaceMethodrefInfo
        12 -> parseConstantNameAndTypeInfo
        15 -> parseConstantMethodHandleInfo
        16 -> parseConstantMethodTypeInfo
        18 -> parseConstantInvokeDynamicInfo
        _ -> fail ("Unknown tag: " ++ show tag)
    
parseConstantUtf8Info :: Get CpInfo
parseConstantUtf8Info = do
    tag <- getWord8
    length <- getWord16be
    bytes <- getLazyByteString (fromIntegral length)
    return (ConstantUtf8Info tag length bytes)

parseConstantIntegerInfo :: Get CpInfo
parseConstantIntegerInfo = do
    tag <- getWord8
    value <- getWord32be
    return (ConstantIntegerInfo tag value)

parseConstantFloatInfo :: Get CpInfo
parseConstantFloatInfo = do
    tag <- getWord8
    value <- getWord32be
    return (ConstantFloatInfo tag value)

parseConstantLongInfo :: Get CpInfo
parseConstantLongInfo = do
    tag <- getWord8
    highBytes <- getWord32be
    lowBytes <- getWord32be
    return (ConstantLongInfo tag highBytes lowBytes)

parseConstantDoubleInfo :: Get CpInfo
parseConstantDoubleInfo = do
    tag <- getWord8
    highBytes <- getWord32be
    lowBytes <- getWord32be
    return (ConstantDoubleInfo tag highBytes lowBytes)

parseConstantClassInfo :: Get CpInfo
parseConstantClassInfo = do
    tag <- getWord8
    nameIndex <- getWord16be
    return (ConstantClassInfo tag nameIndex)

parseConstantStringInfo :: Get CpInfo
parseConstantStringInfo = do
    tag <- getWord8
    stringIndex <- getWord16be
    return (ConstantStringInfo tag stringIndex)

parseConstantFieldrefInfo :: Get CpInfo
parseConstantFieldrefInfo = do
    tag <- getWord8
    classIndex <- getWord16be
    nameAndTypeIndex <- getWord16be
    return (ConstantFieldrefInfo tag classIndex nameAndTypeIndex)

parseConstantMethodrefInfo :: Get CpInfo
parseConstantMethodrefInfo = do
    tag <- getWord8
    classIndex <- getWord16be
    nameAndTypeIndex <- getWord16be
    return (ConstantMethodrefInfo tag classIndex nameAndTypeIndex)

parseConstantInterfaceMethodrefInfo :: Get CpInfo
parseConstantInterfaceMethodrefInfo = do
    tag <- getWord8
    classIndex <- getWord16be
    nameAndTypeIndex <- getWord16be
    return (ConstantInterfaceMethodrefInfo tag classIndex nameAndTypeIndex)

parseConstantNameAndTypeInfo :: Get CpInfo
parseConstantNameAndTypeInfo = do
    tag <- getWord8
    nameIndex <- getWord16be
    descriptorIndex <- getWord16be
    return (ConstantNameAndTypeInfo tag nameIndex descriptorIndex)

parseConstantMethodHandleInfo :: Get CpInfo
parseConstantMethodHandleInfo = do
    tag <- getWord8
    referenceKind <- getWord8
    referenceIndex <- getWord16be
    return (ConstantMethodHandleInfo tag referenceKind referenceIndex)

parseConstantMethodTypeInfo :: Get CpInfo
parseConstantMethodTypeInfo = do
    tag <- getWord8
    descriptorIndex <- getWord16be
    return (ConstantMethodTypeInfo tag descriptorIndex)

parseConstantInvokeDynamicInfo :: Get CpInfo
parseConstantInvokeDynamicInfo = do
    tag <- getWord8
    bootstrapMethodAttrIndex <- getWord16be
    nameAndTypeIndex <- getWord16be
    return (ConstantInvokeDynamicInfo tag bootstrapMethodAttrIndex nameAndTypeIndex)

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

parseConstantValueAttribute :: Get AttributeInfo
parseConstantValueAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    constantValueIndex <- getWord16be
    return (ConstantValueAttribute{
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

parseCodeAttribute :: ConstantPool -> Get AttributeInfo
parseCodeAttribute constantPool = do
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
    return (CodeAttribute {
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
    
parseLineNumberTableAttribute :: Get AttributeInfo
parseLineNumberTableAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    lineNumberTableLength <- getWord16be
    lineNumberTable <- replicateM (fromIntegral lineNumberTableLength) parseLineNumberTableEntry
    return (LineNumberTableAttribute {
        ltNameIndex = nameIndex, 
        ltLength = length, 
        ltLineNumberTable = lineNumberTable
    })

parseSourceFileAttribute :: Get AttributeInfo
parseSourceFileAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    index <- getWord16be
    return (SourceFileAttribute {
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
    else fail ("Unrecognized frame type: " ++ show frameType)    

parseStackMapTableAttribute :: Get AttributeInfo
parseStackMapTableAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    numberOfEntries <- getWord16be
    stackMapTable <- replicateM (fromIntegral numberOfEntries) parseStackMapTableEntry
    return (StackMapTableAttribute {
        smNameIndex = nameIndex,
        smLength = length,
        smTable = stackMapTable
    })

parseExceptionsAttribute :: Get AttributeInfo
parseExceptionsAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    numberOfExceptions <- getWord16be
    indexTable <- replicateM (fromIntegral numberOfExceptions) getWord16be
    return (ExceptionsAttribute {
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

parseInnerClassesAttribute :: Get AttributeInfo
parseInnerClassesAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    numberOfClasses <- getWord16be
    classes <- replicateM (fromIntegral numberOfClasses) parseInnerClass
    return (InnerClassesAttribute {
        icNameIndex = nameIndex,
        icLength = length,
        icClasses = classes
    })

parseEnclosingMethodAttribute :: Get AttributeInfo
parseEnclosingMethodAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    classIndex <- getWord16be
    methodIndex <- getWord16be
    return (EnclosingMethodAttribute {
        emNameIndex = nameIndex,
        emLength = length,
        emClassIndex = classIndex,
        emMethodIndex = methodIndex
    })

parseSignatureAttribute :: Get AttributeInfo
parseSignatureAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    signatureIndex <- getWord16be
    return (SignatureAttribute {
        sNameIndex = nameIndex,
        sLength = length,
        sSignatureIndex = signatureIndex
    })

parseSyntheticAttribute :: Get AttributeInfo
parseSyntheticAttribute = do
    nameIndex <- getWord16be
    length <- getWord32be
    return (SyntheticAttribute { synNameIndex = nameIndex, synLength = length })

parseAttributeInfo :: ConstantPool -> Get AttributeInfo
parseAttributeInfo constantPool = do
    nameIndex <- lookAhead getWord16be
    let attributeCpInfo = getCpInfoAtIndex constantPool (fromIntegral nameIndex)
    let attributeName = case attributeCpInfo of
                            ConstantUtf8Info _ _ byteString -> Char8.unpack byteString
                            x -> fail ("Expected entry of type ConstantUtf8Info but got: " ++ show x)
    case attributeName of
        "ConstantValue" -> parseConstantValueAttribute
        "Code" -> parseCodeAttribute constantPool
        "LineNumberTable" -> parseLineNumberTableAttribute
        "SourceFile" -> parseSourceFileAttribute
        "StackMapTable" -> parseStackMapTableAttribute
        "Exceptions" -> parseExceptionsAttribute
        "InnerClasses" -> parseInnerClassesAttribute
        "EnclosingMethod" -> parseEnclosingMethodAttribute
        "Signature" -> parseSignatureAttribute
        "Synthetic" -> parseSyntheticAttribute
        s -> fail ("Unrecognized attribute name: " ++ show s)

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

