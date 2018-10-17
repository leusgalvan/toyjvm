module Main(main) where

import ClassFileParser
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy as BL hiding (putStrLn)

main :: IO ()
main = do
    let result = runGetOrFail parseMagicNumber (encode (0xCAFEBABE :: Word32))
    case result of 
      Left (_, _, s) -> putStrLn s
      Right _ -> putStrLn "Success"
