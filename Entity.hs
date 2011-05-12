{-# LANGUAGE Arrows #-}

module Entity (
    Direction (..), 
    Key (..),
    PlayerKey,
    Player,
    Vector, (^*^),
    tankEntity,
    projectileEntity,
    wallEntity,
    WorldInput (..), WorldOutput (..),
    Entity (..), Collision (..), EntityInput (..), EntityOutput (..), EntityState (..),
    module FRP.Yampa.Vector2,
    module FRP.Yampa.VectorSpace
    ) where

import FRP.Yampa
import FRP.Yampa.Event
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import FRP.Yampa.Utilities
import IdentityList

data Direction = North | South | East | West deriving (Eq, Ord, Show)
type Player = Int
data Key = KeyDirection Direction | KeyBreak | KeyFire deriving (Eq, Ord, Show)
type PlayerKey = (Key, Int)
type Vector = Vector2 Double

data WorldInput = WorldInput {
    keyPress :: Maybe PlayerKey
    }

data WorldOutput = WorldOutput {
    entityStates :: [EntityState]
    }

type Entity = SF EntityInput EntityOutput

data Collision = Collision {
    colliders :: [(ILKey, EntityState)],
    displacement :: Vector
    }

data EntityInput = EntityInput {
    keyPressed :: Event PlayerKey,
    collision :: Event Collision
    }

data EntityOutput = EntityOutput {
    entityState :: EntityState, 
    killRequest :: Event (), 
    spawnRequest :: Event [Entity]
    }

data EntityState = 
    Tank {
        entityPosition :: Vector,
        entityVelocity :: Vector,
        entityDirection :: Direction,
        entityPlayer :: Player,
        entitySize :: Vector,
        isSolid :: Bool
    } |
    Wall {
        entityPosition :: Vector,
        entityVelocity :: Vector,
        entityDirection :: Direction,
        entitySize :: Vector,
        isSolid :: Bool
    } |
    Projectile {
        entityPosition :: Vector,
        entityPlayer :: Player,
        entitySize :: Vector,
        isSolid :: Bool
    }
    
projectileEntity :: Player -> Vector -> Direction -> Entity
projectileEntity player position0 direction = 
    proc (EntityInput { collision = collisionEvent }) -> do
        event <- delayEvent 0.01 -< collisionEvent
        let velocity = 30 *^ directionVector direction
        position <- (position0 ^+^) ^<< integral -< velocity
        returnA -< EntityOutput {
            entityState = Projectile {
                entityPosition = position,
                entityPlayer = player,
                entitySize = vector2 1 1,
                isSolid = False
                },
            killRequest = event `tag` (),
            spawnRequest = NoEvent
            }


tankEntity :: Player -> Vector -> Direction -> Entity
tankEntity player position0 direction0 = 
    proc (EntityInput { keyPressed = keyPressedEvent, collision = collisionEvent }) -> do
        (velocity, direction) <- accumHoldBy act (vector2 0 0, direction0) -< keyPressedEvent
        event <- delayEvent 0.0001 -< collisionEvent
        position <- (position0 ^+^) ^<< impulseIntegral -< (velocity, fmap displacement event)
        returnA -< EntityOutput {
            entityState = Tank {
                entityPosition = position,
                entityVelocity = velocity,
                entityDirection = direction,
                entityPlayer = player,
                entitySize = vector2 3 3,
                isSolid = True
                },
            killRequest = case event of
                Event Collision { colliders = colliders' } | or [True | (_, Projectile {}) <- colliders'] -> Event ()
                _ -> NoEvent,
            spawnRequest = case keyPressedEvent of
                Event (KeyFire, player') | player' == player -> Event [
                    projectileEntity player (position ^+^ (2 *^ directionVector direction)) direction
                    ]
                _ -> NoEvent
            }
    where
        act :: (Vector, Direction) -> PlayerKey -> (Vector, Direction)
        act (velocity, oldDirection) (KeyDirection direction, player') | player' == player = 
            (15 *^ directionVector direction, direction)
        act (_, oldDirection) (KeyBreak, player') | player' == player = 
            (vector2 0 0, oldDirection)
        act old _ = old


wallEntity :: (Vector, Vector) -> Entity
wallEntity (v1, v2) = proc _ -> returnA -< EntityOutput {
    entityState = Wall {
        entityPosition = 0.5 *^ (v1 ^+^ v2),
        entityVelocity = zeroVector,
        entityDirection = North,
        entitySize = v2 ^-^ v1,
        isSolid = True
        },
    killRequest = NoEvent,
    spawnRequest = NoEvent
    }
    

directionVector :: Direction -> Vector
directionVector North = vector2 0 1
directionVector South = vector2 0 (-1)
directionVector East = vector2 1 0
directionVector West = vector2 (-1) 0

v1 ^*^ v2 = vector2 (vector2X v1 * vector2X v2) (vector2Y v1 * vector2Y v2)

