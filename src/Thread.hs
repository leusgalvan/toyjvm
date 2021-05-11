module Thread where

import Frame

type ThreadId = Int

data Thread = Thread {
    pc :: Int,
    stack :: [Frame]
}