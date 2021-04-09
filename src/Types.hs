module Types where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color

-- Types and constants
data MoveDirection
  = R
  | L
  | U
  | D
  | None
  deriving (Eq)

data CellType
    = Bx
    | Fi
    | Fl
    | Pl
    | Ds
    | Wl
    deriving (Eq)

data GameState = GameState
  { position :: Point,
    direction :: MoveDirection,
    currentLevel :: Level,
    speed :: Float
  }

type Cell = (Point, CellType)

type Level = [Cell]

tileSize :: Float
tileSize = 32.0

fps :: Int
fps = 60

background :: Color 
background = black