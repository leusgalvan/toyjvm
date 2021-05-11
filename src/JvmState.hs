module JvmState where

import qualified Data.Map as M
import Thread as T
import MethodArea as MA
import Heap as H
import Class

data JvmState = JvmState {
    heap :: Heap,
    classes :: M.Map String Class,
    currentThread :: ThreadId,
    threads :: M.Map ThreadId Thread
}

init :: JvmState
init = JvmState {
    heap = H.empty,
    classes = M.empty,
    currentThread = 1,
    threads = M.singleton 1 (T.create 1 "main")
}