{-# LANGUAGE Arrows #-}

import FRP.Yampa hiding (first)
import FRP.Yampa.Event
import FRP.Yampa.Vector2
import Data.Time.Clock
import Data.IORef
import Debug.Trace

import World
import World.NCurses
import World.Vty
import Entity
import IdentityList

main :: IO ()
main = runWorld (gameLoop :: VtyWorld -> IO ())

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


game :: SF WorldInput WorldOutput
game = proc worldInput -> do
    rec
        entityOutputs <- gameCore entities0 -< (worldInput, entityOutputs)
    returnA -< WorldOutput {
        entityStates = map entityState (elemsIL entityOutputs)
        }
    where
        entities0 = listToIL [
            tankEntity 1 (vector2 20 10) East,
            tankEntity 2 (vector2 40 40) West,
            wallEntity (vector2 0 0, vector2 5 100),
            wallEntity (vector2 0 0, vector2 300 5),
            wallEntity (vector2 55 55, vector2 60 60)
            ]

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
            let displacement' = if isSolid s1 && isSolid s2
                    then
                        let t = tankCollisionDisplacement (frontCollisonSideVelocity s1 s2) (frontCollisonSideVelocity s2 s1) in
                        t *^ entityVelocity s1
                    else zeroVector in
            (k1, Collision { colliders = [(k2, s2)], displacement = displacement'})
                

        frontPositionVelocity state = edgePositionVelocity (entityDirection state) state

        edgePositionVelocity :: Direction -> EntityState -> (Double, Double)
        edgePositionVelocity direction state = 
            let position = entityPosition state in
            case direction of
                North -> (vector2Y position + 0.5 * vector2Y (entitySize state), vector2Y (entityVelocity state))
                South -> (vector2Y position - 0.5 * vector2Y (entitySize state), vector2Y (entityVelocity state))
                East -> (vector2X position + 0.5 * vector2X (entitySize state), vector2X (entityVelocity state))
                West -> (vector2X position - 0.5 * vector2X (entitySize state), vector2X (entityVelocity state))
        
        frontCollisonSideVelocity :: EntityState -> EntityState -> (Double, Double, Double)
        frontCollisonSideVelocity s1 s2 = 
            let (p, v) = frontPositionVelocity s1 in
            let (q, _) = edgePositionVelocity (oppositeDirection (entityDirection s1)) s2 in
            (p, q, v)
            
        tankCollisionDisplacement (p1, q1, v1) (p2, q2, v2) =
            if v1 == 0 then 0 else
                let t1 = collisionTime p1 q1 v1 in
                let t2 = collisionTime p2 q2 v2 in
                if t1 < t2
                then 0
                else t1

        collisionTime p q v = (q - p) / v

        collisionPairs :: [((ILKey, EntityState), (ILKey, EntityState))]
        collisionPairs = [(e1, e2) | e1 <- entities, e2 <- entities, fst e1 /= fst e2, overlap (boundingBox (snd e1)) (boundingBox (snd e2))]

        overlap (v1, v2) (u1, u2) = 
            overlapAxis (vector2X v1, vector2X v2) (vector2X u1, vector2X u2) &&
            overlapAxis (vector2Y v1, vector2Y v2) (vector2Y u1, vector2Y u2)

        overlapAxis (start1, end1) (start2, end2) = start1 <= end2 && start2 <= end1

        boundingBox e = (entityPosition e ^-^ 0.5 *^ entitySize e, entityPosition e ^+^ 0.5 *^ entitySize e)


oppositeDirection North = South
oppositeDirection South = North
oppositeDirection East = West
oppositeDirection West = East


