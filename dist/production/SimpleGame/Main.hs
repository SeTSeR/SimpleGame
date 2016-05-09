module Main where

import Control.Monad.Trans

import Data.Array
import Data.IORef
import Data.Text hiding (length, maximum)


import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo

type GameState = (Int, Int, Int, Int)

gameList :: [Int]
gameList = [23, 24, 24, 23, 22, 23, 24, 23, 23, 23, 23, 24, 25, 25, 25, 24, 24, 24, 25, 24, 25, 25, 25, 25, 26, 25, 25, 24, 23, 24, 24, 24, 25, 26, 27, 27, 27, 27, 28, 28, 27, 27, 26, 25, 24, 25, 25, 24, 23, 23, 23, 22, 21, 20, 19, 18, 18, 19, 20, 19, 19, 20, 21, 22, 23, 22, 21, 20, 20, 20, 19, 20, 20, 20, 21, 20, 19, 20, 21, 21, 22, 22, 22, 22, 21, 22, 22, 21, 20, 20, 19, 20, 19, 19, 19, 18, 19, 20, 21, 22]

gameMap :: Array Int Int
gameMap = listArray (0, 99) gameList

gameMax :: Int
gameMax = maximum gameList

initState :: GameState
initState = (1, 0, 0, gameMap!0)

getNextState :: GameState -> GameState
getNextState (vx, vy, x, y) | y + vy >= gameMap!(x + vx) = (vx, vy - 1, x + vx, y + vy)
                            | otherwise = (vx, 0, x, y)

toggleKeyEvent :: DrawingArea -> IORef GameState -> EventM EKey Bool
toggleKeyEvent area state = tryEvent $ do
    window <- liftIO $ widgetGetDrawWindow area
    (w, h) <- liftIO $ widgetGetSize area
    (vx, vy, x, y) <- liftIO $ readIORef state
    liftIO $ putStrLn "ToggleKeyEvent:"
    liftIO $ putStrLn $ show (vx, vy, x, y)
    keyName <- unpack <$> eventKeyName
    case keyName of
        "Up" -> liftIO $ writeIORef state (vx, vy + 1, x, y)
{-      "Down" -> return ()
        "Left" -> return ()
        "Right" -> return ()-}
        otherwise -> liftIO $ putStrLn keyName
    liftIO $ runGame state area >>= putStrLn
    liftIO $ drawWindowInvalidateRect window (Rectangle 0 0 w h) True

runGame :: IORef GameState -> DrawingArea -> IO String
runGame state area = do
    window <- widgetGetDrawWindow area
    (w, h) <- widgetGetSize area
    (vx, vy, x, y) <- readIORef state
    putStrLn "RunGame:"
    putStrLn $ show (vx, vy, x, y)
    if (x + vx < length gameMap) && (y + vy >= gameMap!(x + vx))
        then do
        writeIORef state $ getNextState (vx, vy, x, y)
        answer <- runGame state area
        drawWindowInvalidateRect window (Rectangle 0 0 w h) True
        return (show (x, y) ++ "\n" ++ answer)
    else do
        drawWindowInvalidateRect window (Rectangle 0 0 w h) True
        return (show (x, y))

drawMap :: Int -> Int -> Array Int Int -> IORef GameState -> Render ()
drawMap w h gMap state = do
    let n' = (fromIntegral . length) gMap
        w' = fromIntegral w
        w'' = w'/n'
        h' = fromIntegral h
        m' = fromIntegral gameMax
        r' = w''/2
        height x = (fromIntegral x)*h'/m'*0.8
        drawRects arr m | m < length gMap = do
            setSourceRGB 0 1 0
            rectangle ((fromIntegral m)*w'') (h' - (height (arr!m))) w'' (height (arr!m))
            fill
            drawRects arr (m + 1)
                        | otherwise        = return ()
    setSourceRGB 1 1 1
    rectangle 0 0 w' h'
    fill
    drawRects gMap 0
    (_, _, x, y) <- liftIO $ readIORef state
    setSourceRGB 0 0 1
    arc ((fromIntegral x)*w'' + r') (h' - (height y) - r') r' 0 6.28
    fill

render :: IORef GameState -> DrawingArea -> IO Bool
render state area = do
    win <- widgetGetDrawWindow area
    (w, h) <- widgetGetSize area
    renderWithDrawable win $ drawMap w h gameMap state
    return True

main :: IO ()
main = do
    initGUI
    window <- windowNew
    state <- newIORef initState
    area <- drawingAreaNew
    onExpose area $ const $ render state area
    set window [containerChild := area]
    window `on` keyReleaseEvent $ toggleKeyEvent area state
    window `on` destroyEvent $ liftIO mainQuit >> return False
    widgetShowAll window
    log <- runGame state area
    putStrLn log
    mainGUI