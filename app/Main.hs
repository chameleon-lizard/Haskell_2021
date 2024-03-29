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
  let pos = fst (head (filter (\x -> snd x == Pl) level))

  if length (filter (\x -> snd x == Pl) level) /= 1 || length (filter (\x -> snd x == Ds) level) /= length (filter (\x -> snd x == Bx) level)
    then
      print "Wrong map format."
    else
      play
        window
        background
        fps
        GameState
          { position = pos,
            direction = None,
            currentLevel = changeType level pos Fl,
            speed = 0
          }
        (`render`
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
