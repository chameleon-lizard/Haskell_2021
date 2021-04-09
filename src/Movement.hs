module Movement where

import Collisions
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.Pure.Game
import Types

-------------------------------------------------------------------------------

changeType :: Level -> Point -> CellType -> Level
changeType level pt cell = map (\((x, y), tileType) -> if (x == fst pt) && (y == snd pt) then ((x, y), cell) else ((x, y), tileType)) level

moveBox :: Point -> MoveDirection -> Level -> Level
moveBox pt dir level
  | moveCell == Fi && moveToCell == Fl = changeType (changeType level box Ds) box' Bx
  | moveCell == Fi && moveToCell == Ds = changeType (changeType level box Ds) box' Fi
  | moveCell == Bx && moveToCell == Fl = changeType (changeType level box Fl) box' Bx
  | moveCell == Bx && moveToCell == Ds = changeType (changeType level box Fl) box' Fi
  where
    box = fst (head (filter (\((x, y), tileType) -> (tileType == Bx || tileType == Fi) && isHit pt (x, y)) level))
    box'
      | dir == D = (fst box, snd box - tileSize)
      | dir == U = (fst box, snd box + tileSize)
      | dir == L = (fst box - tileSize, snd box)
      | dir == R = (fst box + tileSize, snd box)
      | otherwise = box
    moveToCell = snd (head (filter (\((x, y), tileType) -> x == fst box' && y == snd box') level))
    moveCell = snd (head (filter (\((x, y), tileType) -> x == fst box && y == snd box) level))

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
  gs {direction = L}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
  gs {direction = R}
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gs =
  gs {direction = U}
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gs =
  gs {direction = D}
handleKeys _ gs = gs {direction = None}

checkSpeed :: GameState -> Float
checkSpeed gs
  | direction gs == U || direction gs == R || direction gs == L || direction gs == D = tileSize
  | otherwise = 0

move :: MoveDirection -> GameState -> GameState
move dir gs =
  if not (isCollision gs (fst (position gs) + speedX, snd (position gs) + speedY) Wl)
    then
      ( if ( isCollision
               gs
               (fst (position gs) + speedX, snd (position gs) + speedY)
               Bx
               || isCollision
                 gs
                 (fst (position gs) + speedX, snd (position gs) + speedY)
                 Fi
           )
          && not
            ( isCollision
                gs
                (fst (position gs) + speedX * 2, snd (position gs) + speedY * 2)
                Wl
                || isCollision
                  gs
                  (fst (position gs) + speedX * 2, snd (position gs) + speedY * 2)
                  Bx
                || isCollision
                  gs
                  (fst (position gs) + speedX * 2, snd (position gs) + speedY * 2)
                  Fi
            )
          then
            GameState
              { position = (fst (position gs) + speedX, snd (position gs) + speedY),
                direction = None,
                currentLevel =
                  moveBox
                    (fst (position gs) + speedX, snd (position gs) + speedY)
                    dir
                    (currentLevel gs),
                speed = checkSpeed gs
              }
          else
            if isCollision
              gs
              (fst (position gs) + speedX, snd (position gs) + speedY)
              Bx
              || isCollision
                gs
                (fst (position gs) + speedX, snd (position gs) + speedY)
                Fi
              then gs
              else
                GameState
                  { position = (fst (position gs) + speedX, snd (position gs) + speedY),
                    direction = None,
                    currentLevel = currentLevel gs,
                    speed = checkSpeed gs
                  }
      )
    else gs
  where
    speedX 
      | dir == R = checkSpeed gs
      | dir == L = checkSpeed gs * (-1)
      | otherwise = 0
    speedY
      | dir == U = checkSpeed gs
      | dir == D = checkSpeed gs * (-1)
      | otherwise = 0