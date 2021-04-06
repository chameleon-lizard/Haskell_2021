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

data Heading
  = FL
  | FR
  | FU
  | FD
  deriving (Eq)

data GameState = GameState
  { position :: Point,
    direction :: MoveDirection,
    heading :: Heading,
    currentLevel :: Level,
    spriteCount :: Int,
    speed :: Float
  }

type CellType = Char

type Cell = (Point, CellType)

type Level = [Cell]

tileSize :: Float
tileSize = 32.0

window :: Display
window = InWindow "Dis is gaem" (1024, 768) (0, 0)

background :: Color
background = black

fps :: Int
fps = 60

