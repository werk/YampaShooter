module World.Vty (VtyWorld) where

import World
import World.Ascii
import Entity

import Graphics.Vty hiding (Color, Picture)
import qualified Graphics.Vty as Vty
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad hiding (mapM_, forM_)
import Data.Array.Diff
import Data.Char (toLower)
import Data.Foldable 
import Data.List (groupBy)
import qualified Data.Text as T
import Prelude hiding (mapM_)
import Debug.Trace


data VtyWorld = VtyWorld (TChan PlayerKey) (TVar (Maybe WorldOutput))

instance World VtyWorld where
    input (VtyWorld channel _) = atomically $ do
        empty <- isEmptyTChan channel
        if empty 
            then return (WorldInput { keyPress = Nothing })
            else do
                state <- readTChan channel
                return (WorldInput { keyPress = Just state })
    output (VtyWorld _ variable) state = atomically $ writeTVar variable (Just state)
    runWorld gameFunction = do
        let colors = initializeColors
        channel <- newTChanIO
        stateVariable <- newTVarIO Nothing
        stopChannel <- newTChanIO
        vty <- mkVty
        forkIO $ loop stopChannel $ inputLoop (next_event vty) channel
        forkIO $ loop stopChannel $ drawLoop (terminal vty) colors stateVariable
        result <- gameFunction (VtyWorld channel stateVariable)
        atomically $ replicateM 2 $ writeTChan stopChannel ()
        atomically $ await (isEmptyTChan stopChannel)
        shutdown vty
        return result
        where
            inputLoop :: IO Event -> TChan PlayerKey -> IO ()
            inputLoop input channel = do
                key <- liftM getKey input
                mapM_ (atomically . writeTChan channel) key
            
            drawLoop :: TerminalHandle -> (Color -> Attr) -> TVar (Maybe WorldOutput) -> IO ()
            drawLoop terminal colors stateVariable = do
                state <- atomically $ do
                    state <- readTVar stateVariable
                    case state of
                        Just state -> do
                            writeTVar stateVariable Nothing
                            return state
                        Nothing -> retry
                draw terminal colors state

            loop :: TChan () -> IO () -> IO ()
            loop stopChannel monad = do
                continue <- atomically $ do
                    empty <- isEmptyTChan stopChannel
                    when (not empty) (readTChan stopChannel)
                    return empty
                when continue $ do
                    monad
                    loop stopChannel monad
                
            await monad = do
                flag <- monad
                when (not flag) retry

initializeColors :: Color -> Attr
initializeColors =
    let red' = Attr KeepCurrent (SetTo red) (SetTo black) in
    let green' = Attr KeepCurrent (SetTo green) (SetTo black) in
    let blue' = Attr KeepCurrent (SetTo blue) (SetTo black) in
    let black' = Attr KeepCurrent (SetTo black) (SetTo black) in
    \color -> case color of 
        Transparent -> black'
        Green -> green'
        Blue -> blue'
        _ -> red'

draw :: TerminalHandle -> (Color -> Attr) -> WorldOutput -> IO ()
draw terminal colors worldOutput = do
    region <- display_bounds terminal
    let picture = background (region_width region) (region_height region)
    let picture' = foldl' drawEntity picture (entityStates worldOutput) 
    drawPicture terminal colors picture'

drawPicture :: TerminalHandle -> (Color -> Attr) -> Picture -> IO ()
drawPicture terminal colors picture = do
    bounds <- display_bounds terminal
    display <- display_context terminal bounds
    output_picture display (pic_for_image image)
    where
        image = horiz_cat (map linePicture (groupBy (\a b -> fst (fst a) == fst (fst b)) (assocs picture)))
        linePicture line = vert_cat (map pointPicture line)
        pointPicture ((x, y), (character, color)) = char (colors color) character

getKey :: Event -> Maybe PlayerKey
getKey (EvKey key _) = case key of
    KUp -> Just (KeyDirection North, 1)
    KDown -> Just (KeyDirection South, 1)
    KLeft -> Just (KeyDirection West, 1)
    KRight -> Just (KeyDirection East, 1)
    KHome -> Just (KeyBreak, 1)
    KEnd -> Just (KeyFire, 1)
    KASCII character ->
        case toLower character of
            'r' -> Just (KeyDirection North, 2)
            'f' -> Just (KeyDirection South, 2)
            'd' -> Just (KeyDirection West, 2)
            'g' -> Just (KeyDirection East, 2)
            'q' -> Just (KeyBreak, 2)
            'a' -> Just (KeyFire, 2)
            _ -> Nothing
    _ -> Nothing
getKey _ = Nothing

