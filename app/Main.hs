module Main(main) where

import ClassFileParser
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy as BL hiding (putStrLn)
import GHC.Word

main :: IO ()
main = do
    let result = runGetOrFail parseCpInfo (pack [1, 0, 5, 118, 119, 110, 111, 112])
    case result of 
      Left (_, _, s)  -> putStrLn s
      Right (_, _, s) -> putStrLn (show s)
