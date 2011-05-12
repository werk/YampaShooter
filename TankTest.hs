{-# LANGUAGE Arrows #-}

import FRP.Yampa hiding (first)
import FRP.Yampa.Event
import FRP.Yampa.Vector2
import Data.Time.Clock
import Data.IORef
import Debug.Trace

import World
import World.NCurses
--import World.Vty
import Entity
import IdentityList

main :: IO ()
main = runWorld (gameLoop :: NCursesWorld -> IO ())

gameLoop :: World w => w -> IO ()
gameLoop world = do
    lastTime <- newIORef undefined
    reactimate (init lastTime) (sense lastTime) actuate game
    where
        init :: IORef UTCTime -> IO WorldInput
        init lastTime = do 
            newTime <- getCurrentTime
            writeIORef lastTime newTime
            return WorldInput { keyPress = Nothing }
        sense :: IORef UTCTime -> Bool -> IO (DTime, Maybe WorldInput)
        sense lastTime blocking = do
            key <- input world
            oldTime <- readIORef lastTime
            newTime <- getCurrentTime
            writeIORef lastTime newTime
            let deltaTime = diffTime newTime oldTime
            return $ (deltaTime, Just key)
        actuate :: Bool -> WorldOutput -> IO Bool
        actuate changed state = do
            output world state
            return False

diffTime :: UTCTime -> UTCTime -> DTime
diffTime newTime oldTime = (fromRational . toRational) (diffUTCTime newTime oldTime)

newProjectileState player position direction = EntityState {
    entityType = Projectile 10,
    entityPosition = position ^+^ (2 *^ directionVector direction),
    entityDirection = direction,
    entityPlayer = player,
    entitySize = vector2 1 1,
    isSolid = False
    }
    
newTankState player position direction = EntityState {
    entityType = Tank,
    entityPosition = position,
    entityVelocity = vector2 0 0,
    entityDirection = direction,
    entityPlayer = player,
    entitySize = vector2 3 3,
    entityHealth = 100,
    isSolid = True
    }
    
newWallState point1 point2 = EntityState {
    entityType = Wall,
    entityPosition = 0.5 *^ (point1 ^+^ point2),
    entityVelocity = zeroVector,
    entityDirection = North,
    entitySize = point2 ^-^ point1,
    entityHealth = 1,
    isSolid = True
    }

brickWall :: Vector -> Vector -> [EntityState]
brickWall point1 point2 = 
    [newWallState p1 p2 | 
        x <- [vector2X point1 .. vector2X point2], 
        y <- [vector2Y point1 .. vector2Y point2],
        let d = vector2 0.5 0.5,
        let p1 = vector2 x y ^-^ d,
        let p2 = vector2 x y ^+^ d]
        
game :: SF WorldInput WorldOutput
game = proc worldInput -> do
    rec
        entityOutputs <- gameCore entities0 -< (worldInput, entityOutputs)
    returnA -< WorldOutput {
        entityStates = map entityState (elemsIL entityOutputs)
        }
    where
        entities0 = listToIL $ [
            tankEntity newProjectileState (newTankState 1 (vector2 20 10) East),
            tankEntity newProjectileState (newTankState 2 (vector2 40 40) West),
            wallEntity (newWallState (vector2 0 0) (vector2 5 100)),
            wallEntity (newWallState (vector2 0 0) (vector2 100 5))
            --wallEntity (newWallState (vector2 55 55) (vector2 60 60))
            ] ++ map wallEntity (brickWall (vector2 55 55) (vector2 60 60))

gameCore :: IL Entity -> SF (WorldInput, IL EntityOutput) (IL EntityOutput)
gameCore entities = 
    dpSwitch route entities
        (noEvent --> arr killOrSpawn)
        (\sfs' f -> gameCore (f sfs'))

killOrSpawn :: (a, IL EntityOutput) -> (Event (IL Entity -> IL Entity))
killOrSpawn (_, entityOutputs) = foldl (mergeBy (.)) noEvent es
    where
        es :: [Event (IL Entity -> IL Entity)]
        es = [mergeBy 
                (.) 
                (killRequest entityOutput `tag` (deleteIL key))
                (fmap (foldl (.) id . map insertIL_) (spawnRequest entityOutput))
                | (key, entityOutput) <- assocsIL entityOutputs ]
                 
                 
route :: (WorldInput, IL EntityOutput) -> IL sf -> IL (EntityInput, sf)
route (worldInput', entityOutputs) entities = {-trace (show (length (elemsIL entityOutputs))) $-} mapIL route' entities
    where
        entityStates = mapIL (entityState . snd) entityOutputs
        cs = collisions (assocsIL entityStates)
        route' (k, entity) = -- trace (show (length cs))
            (EntityInput { keyPressed = maybeToEvent (keyPress worldInput'), 
            collision = maybeToEvent (lookup k cs) }
            , entity)


collisions :: [(ILKey, EntityState)] -> [(ILKey, Collision)]
collisions entities = map collision collisionPairs
    where
        collision :: ((ILKey, EntityState), (ILKey, EntityState)) -> (ILKey, Collision)
        collision ((k1, s1), (k2, s2)) =
            (k1, Collision { 
                colliders = [(k2, s2)], 
                displacement = collisionDisplacement s1 s2 
                }
            )
                
        collisionDisplacement :: EntityState -> EntityState -> Vector
        collisionDisplacement s1 s2 =
            let direction1 = entityDirection s1 in
            let projection1 = projection direction1 in
            let front1 = entityPosition s1 ^+^ ((sign direction1 * 0.5) *^ entitySize s1) in
            let collision1 = entityPosition s2 ^-^ ((sign direction1 * 0.5) *^ entitySize s2) in
            let impactTime1 = collisionTime front1 (entityVelocity s1) collision1 projection1 in

            let direction2 = entityDirection s2 in
            let projection2 = projection direction2 in
            let front2 = entityPosition s2 ^+^ ((sign direction2 * 0.5) *^ entitySize s2) in
            let collision2 = entityPosition s1 ^-^ ((sign direction2 * 0.5) *^ entitySize s1) in
            let impactTime2 = collisionTime front2 (entityVelocity s2) collision2 projection2 in

            if (isSolid s1 && isSolid s2) 
                && moving s1 
                && moving s2 `implies` (impactTime1 >= impactTime2)
            then (2 * impactTime1) *^ entityVelocity s1
            else zeroVector

        moving :: EntityState -> Bool
        moving state = entityVelocity state /= zeroVector 

        implies a b = not a || b

        collisionTime :: Vector -> Vector -> Vector -> (Vector -> Double) -> Double
        collisionTime frontPoint velocity collisionPoint projection = 
            (projection collisionPoint - projection frontPoint) / projection velocity

        collisionPairs :: [((ILKey, EntityState), (ILKey, EntityState))]
        collisionPairs = [(e1, e2) | e1 <- entities, e2 <- entities, fst e1 /= fst e2, overlap (boundingBox (snd e1)) (boundingBox (snd e2))]

        overlap (v1, v2) (u1, u2) = 
            overlapAxis (vector2X v1, vector2X v2) (vector2X u1, vector2X u2) &&
            overlapAxis (vector2Y v1, vector2Y v2) (vector2Y u1, vector2Y u2)

        overlapAxis (start1, end1) (start2, end2) = start1 <= end2 && start2 <= end1

        boundingBox e = (entityPosition e ^-^ 0.5 *^ entitySize e, entityPosition e ^+^ 0.5 *^ entitySize e)

        projection North = vector2Y
        projection South = vector2Y
        projection East = vector2X
        projection West = vector2X
        
        sign North = 1
        sign South = -1
        sign East = 1
        sign West = -1
        
        
