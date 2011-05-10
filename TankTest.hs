{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
--module AsciiShooter.Terminal where

import FRP.Yampa hiding (first)
import FRP.Yampa.Event
import FRP.Yampa.Vector2
import Data.Time.Clock
import Data.IORef

import World
import World.NCurses
import Entity

-- reactimate :: IO a -> (Bool -> IO (DTime, Maybe a)) -> (Bool -> b -> IO Bool) -> SF a b -> IO ()

main :: IO ()
main = runWorld (main' :: NCursesWorld -> IO ())

main' :: World w => w -> IO ()
main' world = do
    lastTime <- newIORef undefined
    reactimate (init lastTime) (sense lastTime) actuate (tank (vector2 10 10, North))
    where
        init lastTime = do 
            newTime <- getCurrentTime
            writeIORef lastTime newTime
            return NoEvent
        sense :: IORef UTCTime -> Bool -> IO (DTime, Maybe (Event Key))
        sense lastTime blocking = do
            key <- input world
            oldTime <- readIORef lastTime
            newTime <- getCurrentTime
            writeIORef lastTime newTime
            let deltaTime = diffTime oldTime newTime
            return $ (deltaTime, Just (maybeToEvent key))
        actuate :: Bool -> Tank -> IO Bool
        actuate changed tank = do
            output world tank
            return False

diffTime :: UTCTime -> UTCTime -> DTime
diffTime oldTime newTime = (fromRational . toRational) (diffUTCTime newTime oldTime)
    
tank :: Tank -> SF (Event Key) Tank
tank tank0 = accumHoldBy act tank0
    where
        act :: Tank -> Key -> Tank
        act (position, _) (KeyRotation rotation) = (position ^+^ rotationVector rotation, rotation)

rotationVector :: Rotation -> Vector
rotationVector North = vector2 0 1
rotationVector South = vector2 0 (-1)
rotationVector East = vector2 1 0
rotationVector West = vector2 (-1) 0

