module World.NCurses (NCursesWorld) where

import World
import Entity

import UI.NCurses hiding (Key, Color, Event)
import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad hiding (mapM_, forM_)
import Data.Foldable
import Data.Text hiding (map, take, reverse)
import Data.Array.Diff
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (mapM_)

data NCursesWorld = NCursesWorld (TChan Input) (MVar Output)

data Color = Red deriving (Eq, Ord, Show)
type Picture = DiffArray (Int, Int) (Char, Color)

instance World NCursesWorld where
    input (NCursesWorld channel _) = atomically $ do
        empty <- isEmptyTChan channel
        if empty 
            then return Nothing
            else do
                state <- readTChan channel
                return (Just state)
    output (NCursesWorld _ variable) state = putMVar variable state
    runWorld gameFunction = do
        channel <- newTChanIO
        stateVariable <- newEmptyMVar
        stopVariable <- newEmptyMVar
        forkIO $ runCurses $ do 
            window <- defaultWindow
            loop window channel stateVariable stopVariable
        result <- gameFunction (NCursesWorld channel stateVariable)
        putMVar stopVariable ()
        return result
        where
            loop :: Window -> TChan Key -> MVar Output -> MVar () -> Curses ()
            loop window channel stateVariable stopVariable = do
                continue <- liftIO $ isEmptyMVar stopVariable
                when continue $ do
                    key <- getKey window
                    liftIO $ mapM_ (atomically . writeTChan channel) key
                    state <- liftIO $ tryTakeMVar stateVariable
                    mapM_ (draw window) state
                    loop window channel stateVariable stopVariable

draw :: Window -> Tank -> Curses ()
draw window tank = do 
    red <- newColorID ColorRed ColorBlack 1
    let colors = const red
    (rows, columns) <- screenSize
    let picture = background columns rows
    picture <- return $ drawTank picture tank
    drawPicture window colors picture
        
getKey :: Window -> Curses (Maybe Key)
getKey window = do
    event <- getEvent window (Just 0)
    case event of
        Just (EventSpecialKey key) -> 
            case key of
                KeyUpArrow -> return (Just (KeyDirection North))
                KeyDownArrow -> return (Just (KeyDirection South))
                KeyLeftArrow -> return (Just (KeyDirection West))
                KeyRightArrow -> return (Just (KeyDirection East))
                _ -> return Nothing
        Just (EventCharacter character) -> 
            case character of
                ' ' -> return (Just KeyFire)
                _ -> return Nothing
        _ -> return Nothing
            
background :: Integral a => a -> a -> DiffArray (Int, Int) (Char, Color)     
background width height = 
    listArray ((0, 0), (fromIntegral width - 1, fromIntegral height - 2)) (repeat (' ', Red))

drawPicture :: Window -> (Color -> ColorID) -> Picture -> Curses ()
drawPicture window colors picture = do
    updateWindow window $ do
        forM_ (assocs picture) $ \((x, y), (character, color)) -> do
            moveCursor (fromIntegral y) (fromIntegral x)
            setColor (colors color)
            drawText (pack ([character]))
    render

translateSprite :: (Int, Int) -> [((Int, Int), (Char, Color))] -> [((Int, Int), (Char, Color))]
translateSprite (x, y) sprite = 
    map (first (\(x', y') -> (x + x', y + y'))) sprite
        
        
tankAsciiNorth = [
    " || ",
    "¤||¤",
    "¤()¤",
    "¤--¤"]

tankAsciiEast = [
    "¤¤¤ ",
    "|o==",
    "¤¤¤ "]
            
tankSprite North = toSprite tankAsciiNorth
tankSprite South = toSprite (reverse tankAsciiNorth)
tankSprite West = toSprite (map reverse tankAsciiEast)
tankSprite East = toSprite tankAsciiEast

toSprite :: [String] -> [((Int, Int), (Char, Color))]
toSprite lines = toSpriteLines 0 lines
    where
        toSpriteLines :: Int -> [String] -> [((Int, Int), (Char, Color))]
        toSpriteLines row [] = []
        toSpriteLines row (line : lines) = toSpriteLine 0 row line ++ toSpriteLines (row + 1) lines

        toSpriteLine :: Int -> Int -> String -> [((Int, Int), (Char, Color))]
        toSpriteLine column row [] = []
        toSpriteLine column row (char : line) = ((column, row), (char, Red)) : toSpriteLine (column + 1) row line
            
drawTank :: Picture -> Tank -> Picture
drawTank picture (location, direction) = 
    let (_, height) = snd (bounds picture) in
    let (x, y) = toTuple location in
    let sprite = translateSprite (x, height - y) (tankSprite direction) in
    picture // sprite
    
toTuple :: Vector -> (Int, Int)
toTuple vector = (round (vector2X vector), round (vector2Y vector))

toVector :: (Int, Int) -> Vector
toVector (x, y) = vector2 (fromIntegral x) (fromIntegral y)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

