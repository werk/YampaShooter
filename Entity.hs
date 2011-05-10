module Entity where

import FRP.Yampa.Vector2

data Rotation = North | South | East | West deriving (Eq, Ord, Show)
type Vector = Vector2 Double
type Tank = (Vector, Rotation)

