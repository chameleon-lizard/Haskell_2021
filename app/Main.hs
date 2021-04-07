{-# LANGUAGE UnicodeSyntax #-}

module Main where

-- Imports
import Graphics.Gloss
import Types
import Movement
import Render
import Load

-------------------------------------------------------------------------------

update :: Float -> GameState -> GameState
update _ gs =
  move (direction gs) gs


main :: IO ()
main = do
  -- Reading stuff, not pure
  wallImg <- loadBMP "assets/wall.bmp"
  storageImg <- loadBMP "assets/storage.bmp"
  boxImg <- loadBMP "assets/box.bmp"
  playerImg <- loadBMP "assets/player.bmp"
  floorImg <- loadBMP "assets/floor.bmp"
  rawData <- readFile "assets/level"

  let level = prepareData $ reverse $ lines rawData

  -- Pure stuff again :)
  play
    window
    background
    fps
    GameState
          { position = fst (head (filter (\x -> snd x == '@') level)),
            direction = None,
            currentLevel = level,
            spriteCount = 0,
            heading = FL,
            speed = 0
          }
    ( `render`
        [ wallImg,
          storageImg,
          boxImg,
          floorImg,
          playerImg
        ]
    )
    handleKeys
    update
