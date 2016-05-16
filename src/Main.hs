module Main where

import Graphics.Gloss.Interface.Pure.Game

import System.Random

type Level = [Int]

data GameState = GS
    { level :: [Int]
    , x :: Int
    , y :: Int
    , vx :: Int
    , vy :: Int
    }

levelSize :: (Int, Int)
levelSize@(levelWidth, levelHeight) = (100, 100)

main :: IO ()
main = do
    gen <- getStdGen
    startGame gen

createLevel :: RandomGen g => Int -> g -> Level
createLevel length gen = unfoldList (head $ take 1 $ randomRs (0, 100) gen) (take length $ randomRs (-2, 2) gen)
  where
    unfoldList :: Int -> [Int] -> [Int]
    unfoldList value [] = [value]
    unfoldList value (x:xs) = [value + x] ++ (unfoldList (value + x) xs)

startGame :: StdGen -> IO ()
startGame gen = play (InWindow "Another game" windowSize (240, 160)) (greyN 0.25) 30 (initState gen) renderer handler updater 

windowSize :: (Int, Int)
windowSize = both (* (round cellSize)) levelSize

cellSize :: Float 
cellSize = 15

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

initState :: StdGen -> GameState
initState gen = GS level 0 (1 + (head level)) 4 0
  where
    level = createLevel levelWidth gen

handler :: Event -> world -> world
handler _ id = id

updater :: Float -> world -> world
updater _ id = id

renderer :: world -> Picture
renderer _ = pictures [ uncurry translate (cellToScreen (x, y))
                      $ color white $ rectangleWire cellSize cellSize
                      | x <- [0..levelWidth - 1], y <- [0..levelHeight - 1]]

cellToScreen :: (Int, Int) -> (Float, Float)
cellToScreen = both ((* cellSize) . fromIntegral)
