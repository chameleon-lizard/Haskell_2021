module Movement where

import Collisions
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.Pure.Game
import Types

-------------------------------------------------------------------------------

changeType :: Level -> Point -> CellType -> Level
changeType level pt cell = map (\((x, y), tileType) -> if (x == fst pt) && (y == snd pt) then ((x, y), cell) else ((x, y), tileType)) level

moveBox :: Point -> MoveDirection -> Level -> Level
moveBox pt dir level = changeType (changeType level box '.') box' 'b'
  where
    box = fst (head (filter (\((x, y), tileType) -> tileType == 'b' && isHit pt (x, y)) level))
    box'
      | dir == D = (fst box, snd box - tileSize)
      | dir == U = (fst box, snd box + tileSize)
      | dir == L = (fst box - tileSize, snd box)
      | dir == R = (fst box + tileSize, snd box)
      | otherwise = box

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
  gs {direction = L, heading = FL}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
  gs {direction = R, heading = FR}
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gs =
  gs {direction = U, heading = FU}
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gs =
  gs {direction = D, heading = FD}
handleKeys _ gs = gs {direction = None}

checkSpeed :: GameState -> Float
checkSpeed gs
  | direction gs == U || direction gs == R || direction gs == L || direction gs == D = tileSize
  | otherwise = 0

move :: MoveDirection -> GameState -> GameState
move R gs =
  if not (isCollision gs (fst (position gs) + speed gs, snd (position gs)) '*')
    then
      ( if isCollision
          gs
          (fst (position gs) + speed gs, snd (position gs))
          'b'
          && not
            ( isCollision
                gs
                (fst (position gs) + tileSize * 2, snd (position gs))
                '*'
                || isCollision
                  gs
                  (fst (position gs) + tileSize * 2, snd (position gs))
                  'b'
            )
          then
            GameState
              { position = (fst (position gs) + speed gs, snd (position gs)),
                direction = None,
                heading = heading gs,
                currentLevel =
                  moveBox
                    (fst (position gs) + tileSize, snd (position gs))
                    R
                    (currentLevel gs),
                spriteCount = spriteCount gs,
                speed = checkSpeed gs
              }
          else
            if isCollision
              gs
              (fst (position gs) + speed gs, snd (position gs))
              'b'
              then gs
              else
                GameState
                  { position = (fst (position gs) + speed gs, snd (position gs)),
                    direction = None,
                    heading = heading gs,
                    currentLevel = currentLevel gs,
                    spriteCount = spriteCount gs,
                    speed = checkSpeed gs
                  }
      )
    else gs
move L gs =
  if not (isCollision gs (fst (position gs) + speed gs * (-1), snd (position gs)) '*')
    then
      ( if isCollision
          gs
          (fst (position gs) + speed gs * (- 1), snd (position gs))
          'b'
          && not
            ( isCollision
                gs
                (fst (position gs) + tileSize * (-2), snd (position gs))
                '*'
                || isCollision
                  gs
                  (fst (position gs) + tileSize * (-2), snd (position gs))
                  'b'
            )
          then
            GameState
              { position =
                  ( fst (position gs) + speed gs * (- 1),
                    snd (position gs)
                  ),
                direction = None,
                heading = heading gs,
                currentLevel =
                  moveBox
                    (fst (position gs) + tileSize * (- 1), snd (position gs))
                    L
                    (currentLevel gs),
                spriteCount = spriteCount gs,
                speed = checkSpeed gs
              }
          else
            if isCollision
              gs
              (fst (position gs) + speed gs * (-1), snd (position gs))
              'b'
              then gs
              else
                GameState
                  { position =
                      ( fst (position gs) + speed gs * (- 1),
                        snd (position gs)
                      ),
                    direction = None,
                    heading = heading gs,
                    currentLevel = currentLevel gs,
                    spriteCount = spriteCount gs,
                    speed = checkSpeed gs
                  }
      )
    else gs
move U gs =
  if not (isCollision gs (fst (position gs), snd (position gs) + speed gs) '*')
    then
      ( if isCollision
          gs
          (fst (position gs), snd (position gs) + speed gs)
          'b'
          && not
            ( isCollision
                gs
                (fst (position gs), snd (position gs) + tileSize * 2)
                '*'
                || isCollision
                  gs
                  (fst (position gs), snd (position gs) + tileSize * 2)
                  'b'
            )
          then
            GameState
              { position = (fst (position gs), snd (position gs) + speed gs),
                direction = None,
                heading = heading gs,
                currentLevel =
                  moveBox
                    (fst (position gs), snd (position gs) + tileSize)
                    U
                    (currentLevel gs),
                spriteCount = spriteCount gs,
                speed = checkSpeed gs
              }
          else
            if isCollision
              gs
              (fst (position gs), snd (position gs) + speed gs)
              'b'
              then gs
              else
                GameState
                  { position = (fst (position gs), snd (position gs) + speed gs),
                    direction = None,
                    heading = heading gs,
                    currentLevel = currentLevel gs,
                    spriteCount = spriteCount gs,
                    speed = checkSpeed gs
                  }
      )
    else gs
move D gs =
  if not (isCollision gs (fst (position gs), snd (position gs) + speed gs * (-1)) '*')
    then
      ( if isCollision
          gs
          (fst (position gs), snd (position gs) + speed gs * (- 1))
          'b'
          && not
            ( isCollision
                gs
                (fst (position gs), snd (position gs) + tileSize * (-2))
                '*'
                || isCollision
                  gs
                  (fst (position gs), snd (position gs) + tileSize * (-2))
                  'b'
            )
          then
            GameState
              { position =
                  ( fst (position gs),
                    snd (position gs) + speed gs * (- 1)
                  ),
                direction = None,
                heading = heading gs,
                currentLevel =
                  moveBox
                    (fst (position gs), snd (position gs) + tileSize * (-1))
                    D
                    (currentLevel gs),
                spriteCount = spriteCount gs,
                speed = checkSpeed gs
              }
          else
            if isCollision
              gs
              (fst (position gs), snd (position gs) + speed gs * (-1))
              'b'
              then gs
              else
                GameState
                  { position =
                      ( fst (position gs),
                        snd (position gs) + speed gs * (- 1)
                      ),
                    direction = None,
                    heading = heading gs,
                    currentLevel = currentLevel gs,
                    spriteCount = spriteCount gs,
                    speed = checkSpeed gs
                  }
      )
    else gs
move _ gs =
  gs