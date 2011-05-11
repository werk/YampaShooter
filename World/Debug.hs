module World.Debug (DebugWorld) where

import World
import Entity

data DebugWorld

instance World DebugWorld where
    input _ = return (WorldInput { keyPress = Nothing })
    output _ state = do
        print state
    runWorld gameFunction = gameFunction undefined

