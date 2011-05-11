module World where

import Entity

class World a where
    input :: a -> IO WorldInput
    output :: a -> WorldOutput -> IO ()
    runWorld :: (a -> IO b) -> IO b

initialState = (vector2 10 10, North)

