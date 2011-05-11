{-# LANGUAGE Arrows #-}

import FRP.Yampa hiding (first)
import FRP.Yampa.Event
import Data.Time.Clock
import Data.IORef

import World
import World.NCurses
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
            tankEntity 2 (vector2 40 40) West
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
route (worldInput', entityOutputs) entities = mapIL route' entities
    where
        route' (k, entity) = 
            (EntityInput { keyPressed = maybeToEvent (keyPress worldInput'), 
            collision = NoEvent }
            , entity)


collisions :: [(ILKey, EntityState)] -> [(ILKey, Collision)]
collisions entities = 
    
    where
        tankCollision (v1, p1) (v2, p2) 
        collisionTime p1 v1 p2 = (p2 - p1) / v1
        entityPairs = [(e1, e2) | e1 <- entities, e2 <- entities, fst e1 /= fst e2, overlap (snd e1) (snd e2)]
        overlap (v1, v2) (u1, u2) = 
            overlapAxis (vector2X v1, vector2X v2) (vector2X u1, vector2X u2) &&
            overlapAxis (vector2Y v1, vector2Y v2) (vector2Y u1, vector2Y u2)
        overlapAxis (start1, end1) (start2, end2) = start1 <= end2 && start2 <= end1
        boundingBox e = (entityPosition e ^-^ 0.5 *^ entitySize e, entityPosition e ^+^ 0.5 *^ entitySize e)


