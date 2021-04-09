{-# LANGUAGE UnicodeSyntax #-}

module Main where

-- Imports
import Graphics.Gloss
import Load
import Movement
import Render
import Types

-------------------------------------------------------------------------------

update :: Float -> GameState -> GameState
update _ gs = move (direction gs) gs

-- Reading stuff, not pure
main :: IO ()
main = do
  wallImg <- loadBMP "assets/wall.bmp"
  storageImg <- loadBMP "assets/storage.bmp"
  boxImg <- loadBMP "assets/box.bmp"
  finishedImg <- loadBMP "assets/finished.bmp"
  playerImg <- loadBMP "assets/player.bmp"
  floorImg <- loadBMP "assets/floor.bmp"
  splashScreen <- loadBMP "assets/splashScreen.bmp"
  rawData <- readFile "assets/level"

  let level = prepareData $ reverse $ lines rawData

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
          finishedImg,
          floorImg,
          playerImg,
          splashScreen
        ]
    )
    handleKeys
    update
