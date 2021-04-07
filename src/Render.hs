module Render where

import Types
import Graphics.Gloss

-------------------------------------------------------------------------------

whatImg :: Cell -> [Picture] -> Picture
whatImg (_, cellType) [wall, storage, box, floor, _]
    | cellType == 'b' = box
    | cellType == 's' = storage 
    | cellType == '*' = wall
    | otherwise = floor

drawTile :: Cell -> [Picture] -> Picture
drawTile cell imgs =
  uncurry translate (fst cell) (whatImg cell imgs)

render :: GameState -> [Picture] -> Picture
render gs imgs =
  pictures
    ( [drawTile cell imgs | cell <- currentLevel gs]
        ++ [ translate
               (fst (position gs))
               (snd (position gs) + 10)
               (imgs !! 4)
           ]
    )