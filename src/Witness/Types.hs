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

type Vertex a = (Vec2, a)
type Edge a = (Vec2, Orientation, a)
type Cell a = (Vec2, Feature)

type Source = Vec2
type Sink = Vec2

type Path = [(Vec2, Orientation)]
type Solution = Path

type RuleResult = [Vec2]

type Validator a = Puzzle' a -> Solution -> Bool

data Puzzle' a = Grid {
  pDimensions :: Vec2,
  pVertices   :: [Vertex a],
  pEdges      :: [Edge a],
  pCells      :: [Cell a],
  pSources    :: [Source],
  pSinks      :: [Sink],
  pRules      :: [Validator a]
}

type Puzzle = Puzzle' Feature
