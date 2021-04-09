module Render where

import Graphics.Gloss
import Types

-------------------------------------------------------------------------------

window :: Display
window = InWindow "Dis is gaem" (1024, 768) (0, 0)


whatImg :: Cell -> [Picture] -> Picture
whatImg (_, cellType) [wall, storage, box, finished, floor, _, _]
  | cellType == Bx = box
  | cellType == Ds = storage
  | cellType == Fi = finished
  | cellType == Wl = wall
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
    gameEnded = not (any (\x -> snd x == Ds) (currentLevel gs))
