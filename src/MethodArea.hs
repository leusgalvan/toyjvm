module MethodArea where

import Data.ByteString.Lazy as BS

type MethodArea = ByteString

empty :: MethodArea
empty = BS.empty