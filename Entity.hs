module Entity (
    Direction (..), 
    Key (..),
    Vector,
    Tank,
    tank,
    module FRP.Yampa.Vector2,
    module FRP.Yampa.VectorSpace
    ) where

import FRP.Yampa
import FRP.Yampa.Event
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace

data Direction = North | South | East | West deriving (Eq, Ord, Show)
data Key = KeyDirection Direction | KeyFire deriving (Eq, Ord, Show)
type Vector = Vector2 Double
type Tank = (Vector, Direction)

tank :: Tank -> SF (Event Key) Tank
tank tank0 = accumHoldBy act tank0
    where
        act :: Tank -> Key -> Tank
        act (position, _) (KeyDirection direction) = (position ^+^ directionVector direction, direction)

directionVector :: Direction -> Vector
directionVector North = vector2 0 1
directionVector South = vector2 0 (-1)
directionVector East = vector2 1 0
directionVector West = vector2 (-1) 0

