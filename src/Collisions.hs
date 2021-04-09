module Collisions where

import Graphics.Gloss.Data.Point
import Types

-------------------------------------------------------------------------------

isHit :: Point -> Point -> Bool
isHit (b1x, b1y) (b2x, b2y) = b1x == b2x && b1y == b2y

isCollision :: GameState -> Point -> CellType -> Bool
isCollision gs pnt checkType =
  any
    (\((x, y), tileType) -> tileType == checkType && isHit pnt (x, y))
    (currentLevel gs)
