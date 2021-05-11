{-# LANGUAGE NamedFieldPuns #-}

module Frame where

import Data.Binary
import Data.ByteString.Lazy as BS
import Control.Monad.Trans.State as S
import Data.Binary.Get as G
import qualified Data.Array as A
import ClassFile

data Frame = F {
    opStack :: ByteString,
    localVarArray :: A.Array Int Word32,
    constantPool :: ConstantPool
} deriving Show

pushInt :: Int -> Frame -> Frame
pushInt n f@F{opStack} = f { opStack = encode n `append` opStack }

popInt :: Frame -> (Frame, Int)
popInt f@F{opStack} = (f', n)
    where (opStack', n)  = stackPopInt opStack
          f'             = f { opStack = opStack'}

storeInt :: Int -> Frame -> Frame
storeInt idx f@F{opStack, localVarArray} = f { opStack = opStack', localVarArray = localVarArray'}
    where (opStack', n)  = stackPopInt opStack
          localVarArray' = localVarArray A.// [(idx, fromIntegral n)]
          
stackPopInt :: ByteString -> (ByteString, Int)
stackPopInt opStack = flip runGet opStack $ do
    n <- getWord32be
    rem <- getRemainingLazyByteString 
    return (rem, fromIntegral n)