module World where

import Entity

data Key = KeyRotation Rotation | KeyFire deriving (Eq, Ord, Show)

type Input = Key
type Output = Tank

class World a where
    input :: a -> IO (Maybe Input)
    output :: a -> Output -> IO ()
    runWorld :: (a -> IO b) -> IO b

