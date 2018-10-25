module Main(main) where

import ClassFileParser
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL (readFile)
import GHC.Word
import System.Directory
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let mainClass = head args
    runProg mainClass

classNameToPath :: String -> String
classNameToPath className = "app\\" ++ className ++ ".class"

runProg :: String -> IO ()
runProg mainClass = do
    let classFilepath = classNameToPath mainClass
    classFileContents <- BL.readFile classFilepath
    let result = runGetOrFail parseClassFile classFileContents
    case result of
      Left l  -> putStrLn (show l)
      Right r -> putStrLn (show r)
