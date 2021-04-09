module Render where

import Graphics.Gloss
import Types

-------------------------------------------------------------------------------

whatImg :: Cell -> [Picture] -> Picture
whatImg (_, cellType) [wall, storage, box, finished, floor, _, _]
  | cellType == 'b' = box
  | cellType == 's' = storage
  | cellType == 'f' = finished
  | cellType == '*' = wall
  | otherwise = floor

drawTile :: Cell -> [Picture] -> Picture
drawTile cell imgs =
  uncurry translate (fst cell) (whatImg cell imgs)

render :: GameState -> [Picture] -> Picture
render gs imgs =
  if not gameEnded
    then
      pictures
        ( [drawTile cell imgs | cell <- currentLevel gs]
            ++ [ uncurry
                   translate
                   (position gs)
                   (imgs !! (length imgs - 2))
               ]
        )
    else imgs !! (length imgs - 1)
  where
    gameEnded = not (any (\x -> snd x == 's') (currentLevel gs))
