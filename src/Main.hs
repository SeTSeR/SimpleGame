module Main where

import Data.Array

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

import System.Random

type Level = Array Int Int

data GameState = GS
    { level :: Level
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
createLevel length gen = listArray (1, length) $ construct 1 length gen 0
  where
    construct 1 length gen last = let (next, nextGen) = randomR (levelHeight `div` 4, (levelHeight*3) `div` 4) gen
                                  in [next] ++ (construct 2 length nextGen next)
    construct x length gen last | x <= length = let (next, nextGen) = randomR (levelHeight `div` 25, (levelHeight*24) `div` 25) gen
                                                in if (abs (next - last) <= 2)
                                                   then
                                                     [next] ++ (construct (x + 1) length nextGen next)
                                                    else
                                                     construct x length nextGen last

--createLevel :: RandomGen g => Int -> g -> Level
--createLevel length gen = listArray (1, length) $ replicate length 25 

startGame :: StdGen -> IO ()
startGame gen = play (InWindow "Another game" windowSize (0, 0)) (greyN 0.25) fps (initState gen) renderer handler updater 

fps :: Int
fps = 30

windowSize :: (Int, Int)
windowSize = both ((* (round cellSize)) . (+ 1)) levelSize

cellSize :: Float 
cellSize = 15

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

initState :: StdGen -> GameState 
initState gen = GS level 0 (1 + (level!1)) 1 0
  where
    level = createLevel levelWidth gen


--updater :: Float -> world -> world
updater _ gs@GS
  { level = level
  , x = x
  , y = y
  , vx = vx
  , vy = vy
  } | x >= levelWidth = gs
    | (x + vx + 1) >= levelWidth = if (y + vy) >= level!levelWidth
                                   then gs
                                     { x = levelWidth
                                     , y = y + vy
                                     , vy = vy - 1
                                     }
                                   else
                                     if (y + vy) >= level!(x + 1)
                                     then gs
                                       { y = y + vy
                                       , vy = vy - 1
                                       }
                                     else gs
                                       { vy = 0
                                       }

updater _ gs@GS
  { level = level
  , x = x
  , y = y
  , vx = vx
  , vy = vy
  } = if (y + vy) >= level!(x + vx + 1)
      then gs
        { x = x + vx
        , y = y + vy
        , vy = vy - 1
        }
      else
        if (y + vy) >= level!(x + 1)
        then gs
          { y = y + vy
          , vy = vy - 1
          }
        else gs
          { vy = 0 
          }

--renderer :: world -> Picture
renderer gs@GS
  { level = level
  , x = x
  , y = y
  } = let width = cellSize * (fromIntegral levelWidth)
          height = cellSize * (fromIntegral levelHeight)
          currx x = cellSize * (fromIntegral x) + cellSize/2
          curry h = (cellSize * (fromIntegral (level!h)))/2
          currheight h = cellSize * (fromIntegral (level!h))
          curry' y = cellSize * (fromIntegral y) + cellSize/2
      in if x < levelWidth
         then 
           applyViewPortToPicture viewPort $ 
           pictures ([ translate (width/2) (height/2) $ color white $ rectangleSolid width height ] ++
                     [ translate (currx (h - 1)) (curry h) $ color green $ rectangleSolid cellSize (currheight h) | h <- [1..levelWidth]] ++
                     [ translate (currx x) (curry' y) $ color blue $ circleSolid (cellSize/2)])
         else
           applyViewPortToPicture viewPort $ 
           pictures ([ translate (width/2) (height/2) $ color white $ rectangleSolid width height ] ++
                     [ translate (currx (h - 1)) (curry h) $ color green $ rectangleSolid cellSize (currheight h) | h <- [1..levelWidth]] ++
                     [ translate (currx x) (curry' y) $ color blue $ circleSolid (cellSize/2)] ++
                     [ translate (width/2) (height/2) $ color black $ text "Victory!" ])

viewPort :: ViewPort
viewPort = ViewPort (both (negate . (/2)) $ cellToScreen levelSize) 0 1

cellToScreen :: (Int, Int) -> (Float, Float)
cellToScreen = both ((* cellSize) . fromIntegral)

--handler :: Event -> world -> world
handler event gs@GS
  { vx = vx 
  , vy = vy
  } = case event of
    EventKey (SpecialKey KeyUp) Down _ _ -> gs
      { vy = vy + 2 
      }
    EventKey (SpecialKey KeyRight) Down _ _ -> gs
      { vx = vx + 2 
      }
    EventKey (SpecialKey KeyDown) Down _ _ -> gs
      { vy = vy - 2
      }
    EventKey (SpecialKey KeyLeft) Down _ _ -> gs
      { vx = vx - 2
      }
    otherwise -> gs
