module Collisions where

import Graphics.Gloss.Data.Point
import Types

-------------------------------------------------------------------------------

isHit :: Point -> Point -> Bool
isHit (b1x, b1y) (b2x, b2y) =
  (b1x - 10) < b2x + tileSize
    && b1x + 50 - 10 > b2x
    && b1y < b2y + tileSize
    && b1y + 54 > b2y

isCollision :: GameState -> Point -> CellType -> Bool
isCollision gs pnt checkType =
  any
    (\((x, y), tileType) -> tileType == checkType && isHit pnt (x, y))
    (currentLevel gs)
