module Main(main) where

import ClassFileParser
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy as BL hiding (putStrLn)
import GHC.Word
import System.Directory

main :: IO ()
main = do
    currentDir <- getCurrentDirectory
    let testFilePath = currentDir ++ "\\app\\Test.class"
    classFileContents <- BL.readFile(testFilePath)
    let result = runGetOrFail parseClassFile classFileContents
    case result of 
      Left l  -> putStrLn (show l)
      Right r -> putStrLn (show r)
