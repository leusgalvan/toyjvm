module Main(main) where

import ClassFileParser
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy as BL hiding (putStrLn)

main :: IO ()
main = do
    let result = runGetOrFail parseMajorVersion (encode (44 :: Word16))
    case result of 
      Left (_, _, s)  -> putStrLn s
      Right (_, _, s) -> putStrLn (show s)
