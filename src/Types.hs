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

data GameState = GameState
  { position :: Point,
    direction :: MoveDirection,
    currentLevel :: Level,
    speed :: Float
  }

type CellType = Char

type Cell = (Point, CellType)

type Level = [Cell]

tileSize :: Float
tileSize = 32.0

fps :: Int
fps = 60

