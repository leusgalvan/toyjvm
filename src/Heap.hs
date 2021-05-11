module Heap where

import Data.ByteString.Lazy as BS

type Heap = ByteString

empty :: Heap
empty = BS.empty