{-# LANGUAGE Arrows, NamedFieldPuns #-}

module Entity (
    Direction (..), directionVector,  
    Key (..),
    PlayerKey,
    Player,
    Vector, (^*^), 
    tankEntity,
    projectileEntity,
    wallEntity,
    WorldInput (..), WorldOutput (..),
    Entity (..), Collision (..), EntityInput (..), EntityOutput (..), EntityState (..), EntityType (..), 
    module FRP.Yampa.Vector2,
    module FRP.Yampa.VectorSpace
    ) where

import FRP.Yampa
import FRP.Yampa.Event
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import FRP.Yampa.Utilities
import IdentityList
import Data.Maybe   

data Direction = North | South | East | West deriving (Eq, Ord, Show)
type Player = Int
data Key = KeyDirection Direction | KeyBreak | KeyFire deriving (Eq, Ord, Show)
type PlayerKey = (Key, Int)
type Vector = Vector2 Double
type Health = Double
type Damage = Double

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

data EntityType = Tank | Wall | Projectile Damage deriving (Show, Eq, Ord)

data EntityState = EntityState {
        entityType :: EntityType,
        entityPosition :: Vector,
        entityVelocity :: Vector,
        entityDirection :: Direction,
        entityPlayer :: Player,
        entitySize :: Vector,
        entityHealth :: Double,
        isSolid :: Bool
    }
    
isProjectile EntityState { entityType = Projectile _ } = True
isProjectile _ = False

getProjectileDamage EntityState { entityType = Projectile d } = Just d
getProjectileDamage _ = Nothing

collisionDmage :: Collision -> Damage
collisionDmage Collision { colliders } = 
    sum $ catMaybes (map (getProjectileDamage . snd) colliders)

projectileEntity :: EntityState -> Entity
projectileEntity state0 = 
    proc (EntityInput { collision = collisionEvent }) -> do
        event <- delayEvent 0.0001 -< collisionEvent
        let velocity = 30 *^ directionVector (entityDirection state0)
        position <- (entityPosition state0 ^+^) ^<< integral -< velocity
        returnA -< EntityOutput {
            entityState = state0 { entityPosition = position },
            killRequest = event `tag` (),
            spawnRequest = NoEvent
            }

tankEntity :: (Player -> Vector -> Direction -> EntityState) -> EntityState -> Entity
tankEntity cannon state0 = 
    proc (EntityInput { keyPressed = keyPressedEvent, collision = collisionEvent }) -> do
        (velocity, direction) <- accumHoldBy act (vector2 0 0, entityDirection state0) -< keyPressedEvent
        event <- delayEvent 0.0001 -< collisionEvent
        position <- (entityPosition state0 ^+^) ^<< impulseIntegral -< (velocity, fmap displacement event)
        health <- accumHoldBy (-) (entityHealth state0) -< fmap collisionDmage collisionEvent
        die <- edge -< health <= 0
        returnA -< EntityOutput {
            entityState = state0 {
                entityPosition = position,
                entityVelocity = velocity,
                entityDirection = direction
                },
            killRequest = die,
            spawnRequest = case keyPressedEvent of
                Event (KeyFire, player) | player == entityPlayer state0 -> Event [
                    projectileEntity (cannon (entityPlayer state0) position direction)
                    ]
                _ -> NoEvent
            }
    where
        act :: (Vector, Direction) -> PlayerKey -> (Vector, Direction)
        act (velocity, oldDirection) (KeyDirection direction, player) | player == entityPlayer state0 = 
            (15 *^ directionVector direction, direction)
        act (_, oldDirection) (KeyBreak, player) | player == entityPlayer state0 = 
            (vector2 0 0, oldDirection)
        act old _ = old


wallEntity :: EntityState -> Entity
wallEntity state0 = 
    proc (EntityInput { collision = collisionEvent }) -> do
        health <- accumHoldBy (-) (entityHealth state0) -< fmap collisionDmage collisionEvent
        die <- edge -< health <= 0
        returnA -< EntityOutput {
            entityState = state0,
            killRequest = die,
            spawnRequest = NoEvent
            }
        

directionVector :: Direction -> Vector
directionVector North = vector2 0 1
directionVector South = vector2 0 (-1)
directionVector East = vector2 1 0
directionVector West = vector2 (-1) 0

v1 ^*^ v2 = vector2 (vector2X v1 * vector2X v2) (vector2Y v1 * vector2Y v2)

