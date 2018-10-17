module ClassFileParser(
    parseMagicNumber,
    parseMajorVersion,
    parseMinorVersion
) where

import Data.Binary.Get
import Data.Binary

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
        

