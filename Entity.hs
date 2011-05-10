module Entity (
    Direction (..), 
    Tank,
    Vector,
    module FRP.Yampa.Vector2,
    module FRP.Yampa.VectorSpace
    ) where

import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace

data Direction = North | South | East | West deriving (Eq, Ord, Show)
type Vector = Vector2 Double
type Tank = (Vector, Direction)

