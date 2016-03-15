module Witness.Types where

type Vec2 = (Int, Int)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data Orientation
  = Vertical
  | Horizontal
  deriving (Eq, Show)

data Color
  = Black
  | White
  deriving (Eq, Show)

data Alignment
  = Rotatable
  | Fixed
  deriving (Eq, Show)

data Shape = Shape [Vec2]
  deriving (Eq, Show)

data Feature
  = Quarantine Color
  | Tetris Shape Alignment
  | Star Color
  | Negation
  | Eraser
  | Gap
  | Hex
  deriving (Eq, Show)

type Vertex = (Vec2, Feature)
type Edge = (Vec2, Orientation, Feature)
type Cell = (Vec2, Feature)

type Source = Vec2
type Sink = Vec2

type Path = [(Vec2, Orientation)]
type Solution = Path

type RuleResult = [Vec2]

type Validator = Puzzle -> Solution -> Bool

data Puzzle = Grid {
  pDimensions :: Vec2,
  pVertices   :: [Vertex],
  pEdges      :: [Edge],
  pCells      :: [Cell],
  pSources    :: [Source],
  pSinks      :: [Sink],
  pRules      :: [Validator]
}
