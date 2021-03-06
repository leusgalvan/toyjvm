module Main(main) where

import ClassFileParser
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL (readFile)
import GHC.Word
import System.Directory
import System.Environment
import Execution
import Text.Pretty.Simple (pPrint)
import ClassFile

main :: IO ()
main = do
    args <- getArgs
    case args of
        mainClass:_ -> runProg mainClass
        [] -> putStrLn "Please provide the main class name"

classNameToPath :: String -> String
classNameToPath className = "app//" ++ className ++ ".class"

runProg :: String -> IO ()
runProg mainClass = do
    let classFilepath = classNameToPath mainClass
    classFileContents <- BL.readFile classFilepath
    let result = runGetOrFail parseClassFile classFileContents
    case result of
      Left l  -> print l
      Right (_, _, r) -> execute r >> return ()
