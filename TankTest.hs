import FRP.Yampa hiding (first)
import FRP.Yampa.Event
import Data.Time.Clock
import Data.IORef

import World
import World.NCurses
import Entity

main :: IO ()
main = runWorld (game :: NCursesWorld -> IO ())

game :: World w => w -> IO ()
game world = do
    lastTime <- newIORef undefined
    reactimate (init lastTime) (sense lastTime) actuate (tank initialState)
    where
        init :: IORef UTCTime -> IO (Event Input)
        init lastTime = do 
            newTime <- getCurrentTime
            writeIORef lastTime newTime
            return NoEvent
        sense :: IORef UTCTime -> Bool -> IO (DTime, Maybe (Event Input))
        sense lastTime blocking = do
            key <- input world
            oldTime <- readIORef lastTime
            newTime <- getCurrentTime
            writeIORef lastTime newTime
            let deltaTime = diffTime newTime oldTime
            return $ (deltaTime, Just (maybeToEvent key))
        actuate :: Bool -> Output -> IO Bool
        actuate changed state = do
            output world state
            return False

diffTime :: UTCTime -> UTCTime -> DTime
diffTime newTime oldTime = (fromRational . toRational) (diffUTCTime newTime oldTime)

