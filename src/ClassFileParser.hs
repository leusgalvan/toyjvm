module ClassFileParser(parseMagicNumber) where

import Data.Binary.Get

parseMagicNumber :: Get ()
parseMagicNumber = do
    let magicNumber = 0xCAFEBABE
    firstWord <- getWord32be
    if firstWord == magicNumber then return ()
    else fail "Class file does not start with correct magic number"
        

