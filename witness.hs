module Witness where

import Data.Function (on, fix)
import Data.List (intersperse, minimumBy, union, (\\))

{-
There are three address spaces for a grid: cells, edges and vertices. The cell
address space is the natural address space by the row and column of the cell.
For example, if a grid is m x n cells, the top-left cell is at address (0, 0),
the top-right cell is at address (0, n - 1), the bottom right cell is at
address (m - 1, n - 1), and so on.

The vertex address space corresponds to the cell address to the south east of
the vertex. So, for example, in a grid with m x n cells, the vertex at the top
left is at address (0, 0) because the cell to the south east of it is cell
(0, 0). The special case is for vertices at the right or bottom of the grid
because these vertices do not have cells to their south easts. In these cases
the vertices have the address that a cell *would* have if there was one to
their south east. For example, in an m x n grid, the top right vertex has
address (0, n), the bottom right vertex has address (m, n) and so on.

The edge address space is made up of horizontal and vertical edges. For a
horizontal edge, it will have the address of the vertex at its left tip. For a
vertical edge, it will have the address of the vertex at its top tip.cs

v(0,0) ----- e(0,0,h) ----- v(0,1) ----- ... ----- e(0,n-1,h) ----- v(0,n)
    |                         |                                       |
    |                         |                                       |
e(0,0,v)     c(0,0)         e(0,1,v)     ...        c(0,n-s)       e(0,n,v)
    |                         |                                       |
    |                         |                                       |
v(1,0) ----- e(1,0,h) ----- v(1,1) ----- ... ----- e(1,n-1,h) ------ v(1,n)
    |                         |                                       |
    .            .            .                         .             .
    .            .            .                         .             .
    .            .            .                         .             .
    |                         |                                       |
v(m-1,0) -- e(m-1,0,h) -- v(m-2,1) ----- ... ---- e(m-1,n-1,h) ----- v(m-1,n)
    |                         |                                       |
    |                         |                                       |
e(m-1,0,v)   c(m-1,1)      e(m-1,1,v)    ...       c(m-1,n-2)      e(m-1,n,v)
    |                         |                                       |
    |                         |                                       |
v(m,0) ----- e(m,0,h) ----- v(m,1) ----- ... ----- e(m,n-1,h) ------ v(m,n)
-}


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
  deriving (Eq)

instance Show Shape where
  show = showShape

data Feature
  = Quarantine Color
  | Tetris Shape Alignment
  | Star Color
  | Negation
  | Eraser
  | Gap
  | Washer
  deriving (Eq, Show)

type Vertex = (Vec2, Feature)
type Edge = (Vec2, Orientation, Feature)
type Cell = (Vec2, Feature)

type Source = Vec2
type Sink = Vec2

type Path = [(Vec2, Orientation)]
type Solution = Path

type RuleResult = [Vec2]

data Puzzle = Grid {
  dimensions :: Vec2,
  vertices :: [Vertex],
  edges :: [Edge],
  cells :: [Cell],
  sources :: [Source],
  sinks :: [Sink],
  rules :: [Puzzle -> Solution -> RuleResult]
};



shapeDims :: Shape -> Vec2
shapeDims (Shape s) = (maximum $ map fst s, maximum $ map snd s)

showShape :: Shape -> String
showShape (Shape s)
  = (\s -> "\n" ++ s ++ "\n")
  $ concat
  $ intersperse "\n"
  $ map (intersperse ' ')
  $ map showR
  $ enumFromTo 0 maxR
  where
    (maxR, maxC) = shapeDims $ Shape s

    rowN :: Int -> [Vec2]
    rowN row = filter ((== row) . fst) s

    showR :: Int -> String
    showR r = map (b2s . (`elem` (map snd $ rowN r))) $ enumFromTo 0 maxC

    b2s b = if b then '#' else ' '

normalize :: Shape -> Shape
normalize shape@(Shape s) = offset (delR, delC) shape
  where
    delR, delC :: Int
    delR = negate $ minimum $ map fst s
    delC = negate $ minimum $ map snd s


offset :: Vec2 -> Shape -> Shape
offset (offR, offC) (Shape s)
  = Shape
  $ map (\(r, c) -> (r + offR, c + offC)) s

gridPartitions :: Vec2 -> Path -> [Shape]
gridPartitions (h, w) soln = gp [([c], [c])] cs
  where
    (c:cs) = [ (r, c) | r <- [0..(pred h)], c <- [0..(pred w)] ]

    gp :: [([Vec2], [Vec2])] -> [Vec2] -> [Shape]
    
    -- All cells accounted for ==> Done
    gp ps [] = map (Shape . snd) ps

    -- No more frontier in the current partition ==> Start the next one
    gp (p@([], shape):ps) (e:es) = gp (([e], [e]):p:ps) es

    -- Otherwise ==> exapand the first frontier element
    gp (((f:fs), shape):ps) extras = gp ((fs', shape'):ps) extras'
      where
        fs' = fs `union` (expansions \\ shape)
        shape' = shape `union` expansions
        extras' = extras \\ expansions
        expansions = map (cellToThe f) $ free f (h, w) soln

free :: Vec2 -> Vec2 -> Path -> [Direction]
free c@(cr, cc) (h, w) es = concat [north, south, east, west]
  where
    north = if cr > 0 && (not $ (c, Horizontal) `elem` es)
      then [North] else []
    south = if cr < pred h && (not $ ((succ cr, cc), Horizontal) `elem` es)
      then [South] else []
    west = if cc > 0 && (not $ (c, Vertical) `elem` es)
      then [West] else []
    east = if cc < pred w && (not $ ((cr, succ cc), Vertical) `elem` es)
      then [East] else []

cellToThe :: Vec2 -> Direction -> Vec2
cellToThe (r, c) North = (pred r, c)
cellToThe (r, c) South = (succ r, c)
cellToThe (r, c) West = (r, pred c)
cellToThe (r, c) East = (r, succ c)



{- Sample values -}

shapeTDown :: Shape
shapeTDown = Shape [
  (0, 0), (0, 1), (0, 2),
          (1, 1),
          (2, 1)
  ]

shapeS :: Shape
shapeS = Shape [
          (0, 1), (0, 2),
  (1, 0), (1, 1)
  ]

