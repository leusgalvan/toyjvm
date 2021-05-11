module ClassLoader where

import ClassFileParser as P
import Data.Binary.Get as G
import Data.ByteString.Lazy as BS

loadClass :: String -> IO ClassFile
loadClass qualifiedClassName = fmap (G.runGet P.parseClassFile) bytesM
    where bytesM = BS.readFile path
          path  = toFilePath qualifiedClassName

-- TODO: Of course this will have to change
toFilePath :: String -> String
toFilePath qualifiedClassName = "app//" ++ qualifiedClassName ++ ".class"