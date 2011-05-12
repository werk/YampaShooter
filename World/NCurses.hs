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
import Data.Char (toLower)
import Data.Foldable 
import qualified Data.Text as T
import Data.Array.Diff
import Data.Set (Set)
import Data.List (transpose)
import qualified Data.Set as Set
import Prelude hiding (mapM_, maximum)

data NCursesWorld = NCursesWorld (TChan PlayerKey) (MVar WorldOutput)

data Color = Red | Green | Blue | Transparent deriving (Eq, Ord, Show)
type Picture = DiffArray (Int, Int) (Char, Color)
data Sprite = Sprite Int Int [((Int, Int), (Char, Color))]

instance World NCursesWorld where
    input (NCursesWorld channel _) = atomically $ do
        empty <- isEmptyTChan channel
        if empty 
            then return (WorldInput { keyPress = Nothing })
            else do
                state <- readTChan channel
                return (WorldInput { keyPress = Just state })
    output (NCursesWorld _ variable) state = putMVar variable state
    runWorld gameFunction = do
        channel <- newTChanIO
        stateVariable <- newEmptyMVar
        stopVariable <- newEmptyMVar
        forkIO $ runCurses $ do 
            window <- defaultWindow
            setEcho False
            colors <- initializeColors
            loop window colors channel stateVariable stopVariable
        result <- gameFunction (NCursesWorld channel stateVariable)
        putMVar stopVariable ()
        return result
        where
            loop :: Window -> (Color -> ColorID) -> TChan PlayerKey -> MVar WorldOutput -> MVar () -> Curses ()
            loop window colors channel stateVariable stopVariable = do
                continue <- liftIO $ isEmptyMVar stopVariable
                when continue $ do
                    key <- getKey window
                    liftIO $ mapM_ (atomically . writeTChan channel) key
                    state <- liftIO $ tryTakeMVar stateVariable
                    mapM_ (draw window colors) state
                    loop window colors channel stateVariable stopVariable

initializeColors :: Curses (Color -> ColorID)
initializeColors = do
    red <- newColorID ColorRed ColorBlack 1
    green <- newColorID ColorGreen ColorBlack 2
    black <- newColorID ColorBlack ColorBlack 3
    blue <- newColorID ColorBlue ColorBlack 4
    return $ \color -> case color of 
        Transparent -> black
        Green -> green
        Blue -> blue
        _ -> red

draw :: Window -> (Color -> ColorID) -> WorldOutput -> Curses ()
draw window colors worldOutput = do
    (rows, columns) <- screenSize
    let picture = background columns rows
    let picture' = foldl' drawEntity picture (entityStates worldOutput) 
    drawPicture window colors picture'

drawEntity :: Picture -> EntityState -> Picture
drawEntity picture state = 
    case entityType state of
        Tank -> 
            drawTank picture (playerColor (entityPlayer state)) (entityPosition state, entityDirection state)
        Projectile _ -> 
            drawProjectile picture (playerColor (entityPlayer state)) (entityPosition state)
        Wall -> 
            drawWall picture (entityPosition state) (entitySize state)

playerColor 1 = Red
playerColor 2 = Green

getKey :: Window -> Curses (Maybe PlayerKey)
getKey window = do
    event <- getEvent window (Just 0)
    case event of
        Just (EventSpecialKey key) -> 
            case key of
                KeyUpArrow -> return (Just (KeyDirection North, 1))
                KeyDownArrow -> return (Just (KeyDirection South, 1))
                KeyLeftArrow -> return (Just (KeyDirection West, 1))
                KeyRightArrow -> return (Just (KeyDirection East, 1))
                KeyHome -> return (Just (KeyBreak, 1))
                KeyEnd -> return (Just (KeyFire, 1))
                _ -> return Nothing
        Just (EventCharacter character) -> 
            case toLower character of
                'r' -> return (Just (KeyDirection North, 2))
                'f' -> return (Just (KeyDirection South, 2))
                'd' -> return (Just (KeyDirection West, 2))
                'g' -> return (Just (KeyDirection East, 2))
                'q' -> return (Just (KeyBreak, 2))
                'a' -> return (Just (KeyFire, 2))
                _ -> return Nothing
        _ -> return Nothing
            
background :: Integral a => a -> a -> DiffArray (Int, Int) (Char, Color)     
background width height = 
    -- Workaround -2 because drawing on the bottom edge breaks NCurses
    listArray ((0, 0), (fromIntegral width - 1, fromIntegral height - 2)) (repeat (' ', Transparent))

drawPicture :: Window -> (Color -> ColorID) -> Picture -> Curses ()
drawPicture window colors picture = do
    updateWindow window $ do
        forM_ (assocs picture) $ \((x, y), (character, color)) -> do
            moveCursor (fromIntegral y) (fromIntegral x)
            setColor (colors color)
            drawText (T.pack ([character]))
    render

translatePoints :: (Int, Int) -> [((Int, Int), (Char, Color))] -> [((Int, Int), (Char, Color))]
translatePoints (x, y) sprite = 
    map (first (\(x', y') -> (x + x', y + y'))) sprite

projectileAscii = [
    "*"
    ]        
        
tankAsciiNorth = [
    " | ",
    "¤|¤",
    "¤O¤",
    "¤-¤",
    "   "]

tankAsciiEast = [
    " ¤¤¤ ",
    " |o==",
    " ¤¤¤ "]

tankSprite North = toSprite tankAsciiNorth
tankSprite South = toSprite (reverse tankAsciiNorth)
tankSprite West = toSprite (map reverse tankAsciiEast)
tankSprite East = toSprite tankAsciiEast

toSprite :: [String] -> Color -> Sprite 
toSprite lines color = 
    Sprite 
        (width lines) 
        (width (transpose lines)) 
        (filter ((/= ' ') . fst . snd) (toSpriteLines 0 lines))
    where
        width lines = maximum (0 : map length lines)

        toSpriteLines :: Int -> [String] -> [((Int, Int), (Char, Color))]
        toSpriteLines row [] = []
        toSpriteLines row (line : lines) = toSpriteLine 0 row line ++ toSpriteLines (row + 1) lines

        toSpriteLine :: Int -> Int -> String -> [((Int, Int), (Char, Color))]
        toSpriteLine column row [] = []
        toSpriteLine column row (char : line) = ((column, row), (char, color)) : toSpriteLine (column + 1) row line

drawProjectile :: Picture -> Color -> Vector -> Picture
drawProjectile picture playerColor location = 
    drawSprite picture location (toSprite projectileAscii playerColor)

drawTank :: Picture -> Color -> (Vector, Direction) -> Picture
drawTank picture playerColor (location, direction) = 
    drawSprite picture location (tankSprite direction playerColor)

drawWall :: Picture -> Vector -> Vector -> Picture
drawWall picture position size = 
    let (width, height) = (round (vector2X size), round (vector2Y size)) in
    let spriteLines = replicate height (replicate width '#') in
    let sprite = toSprite spriteLines Blue in
    drawSprite picture position sprite

drawSprite :: Picture -> Vector -> Sprite -> Picture
drawSprite picture location (Sprite width height points) = 
    let ((x1, y1), (x2, y2)) = bounds picture in
    let withinBounds (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2 in
    let (x, y) = toTuple location in
    let points' = translatePoints (x - width `div` 2, y2 - y - height `div` 2) points in
    picture // filter (withinBounds . fst) points'
    
toTuple :: Vector -> (Int, Int)
toTuple vector = (round (vector2X vector), round (vector2Y vector))

toVector :: (Int, Int) -> Vector
toVector (x, y) = vector2 (fromIntegral x) (fromIntegral y)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

