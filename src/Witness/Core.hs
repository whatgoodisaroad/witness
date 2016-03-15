module Witness.Core (
    module Witness.Types,

    shapeDims,
    normalize,
    offset,
    gridPartitions,
    freeC,
    available,
    cellToThe,
    validatePath,
    verticesOfEdge,
    verticesOfPath,
    edgeWithin,

    cellAt,
    vertexAt,
    edgeAt,

    edgesWhere,
    verticesWhere,
    anyRepeated,

    hexRule -- Todo: move
  ) where

import Witness.Types

import Data.Function (on)
import Data.List (intersperse, minimumBy, union, find, nub, (\\))

--  Find the size of a shape:
shapeDims :: Shape -> Vec2
shapeDims s = (maximum $ map fst s', maximum $ map snd s')
  where
    (Shape s') = normalize s

--  Normalize a shape by shifting it back to the origin:
normalize :: Shape -> Shape
normalize shape@(Shape s) = offset (delR, delC) shape
  where
    delR, delC :: Int
    delR = negate $ minimum $ map fst s
    delC = negate $ minimum $ map snd s

--  Translate the shape by deltas in both directions:
offset :: Vec2 -> Shape -> Shape
offset (offR, offC) (Shape s)
  = Shape
  $ map (\(r, c) -> (r + offR, c + offC)) s

--  Given a grid size and a path within that grid, give the shapes of the
--  partitions that are separated by that path.
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
        expansions = map (cellToThe f) $ freeC f (h, w) soln

--  Given a cell address the dimensions of the grid that the cell is in, and a
--  path within that grid, give the directions one can travel from that cell
--  which do not cross the stroke of a path or extend beyond the limits of the
--  grid.
freeC :: Vec2 -> Vec2 -> Path -> [Direction]
freeC c@(cr, cc) (h, w) es = concat [north, south, east, west]
  where
    north = if cr > 0 && (not $ (c, Horizontal) `elem` es)
      then [North] else []
    south = if cr < pred h && (not $ ((succ cr, cc), Horizontal) `elem` es)
      then [South] else []
    west = if cc > 0 && (not $ (c, Vertical) `elem` es)
      then [West] else []
    east = if cc < pred w && (not $ ((cr, succ cc), Vertical) `elem` es)
      then [East] else []

available :: Vec2 -> Puzzle -> Path -> [Direction]
available v@(vr, vc) p es = concat [north, south, east, west]
  where
    (h, w) = pDimensions p
    vs = verticesOfPath es
    noGap r c o = (/= Just Gap) $ edgeAt p (r, c) o
    north = 
      if vr > 0 
      &&  (not $ (pred vr, vc) `elem` vs)
      && noGap (pred vr) vc Vertical
        then [North] else []
    south =
      if vr < h
      && (not $ (succ vr, vc) `elem` vs)
      && noGap (succ vr) vc Vertical
        then [South] else []
    west =
      if vc > 0
      && (not $ (vr, pred vc) `elem` vs)
      && noGap vr (pred vc) Horizontal
        then [West] else []
    east =
      if vc < w
      && (not $ (vr, succ vc) `elem` vs)
      && noGap vr (succ vc) Horizontal
        then [East] else []

--  Given a cell address and a direction, give the address of the cell to be
--  found in that direction.
cellToThe :: Vec2 -> Direction -> Vec2
cellToThe (r, c) North  = (pred r, c)
cellToThe (r, c) South  = (succ r, c)
cellToThe (r, c) West   = (r, pred c)
cellToThe (r, c) East   = (r, succ c)

validatePath :: Puzzle -> Path -> Bool
validatePath puzzle path
  =   source
  &&  sink
  &&  allWithin
  &&  noRepeatVertices
  &&  connected
  where
    source = let (v1, v2) = verticesOfEdge $ head path 
      in v1 `elem` pSources puzzle || v2 `elem` pSources puzzle
    sink = let (v1, v2) = verticesOfEdge $ last path 
      in v1 `elem` pSources puzzle || v2 `elem` pSinks puzzle
    allWithin = all (edgeWithin puzzle) path
    noRepeatVertices = let vs = map verticesOfEdge path
      in  (not $ anyRepeated $ map fst vs)
      &&  (not $ anyRepeated $ map snd vs)
    connected = scanP path

    scanP [] = True
    scanP [_] = True
    scanP (e1:e2:es)
      =   (snd $ verticesOfEdge e1) == (fst $ verticesOfEdge e2)
      &&  scanP (e2:es)

verticesOfEdge :: (Vec2, Orientation) -> (Vec2, Vec2)
verticesOfEdge ((r, c), Vertical)   = ((r, c), (succ r, c))
verticesOfEdge ((r, c), Horizontal) = ((r, c), (r, succ c))

verticesOfPath :: Path -> [Vec2]
verticesOfPath = nub . concatMap ((\(v1, v2) -> [v1, v2]) . verticesOfEdge)

edgeWithin :: Puzzle -> (Vec2, Orientation) -> Bool
edgeWithin p ((r, c), o) = ew o
  where
    (sr, sc) = pDimensions p
    ew Vertical   = r >= 0 && r <  sr && c >= 0 && c <= sc
    ew Horizontal = r >= 0 && r <= sr && c >= 0 && c <  sc

cellAt :: Puzzle -> Vec2 -> Maybe Feature
cellAt p a
  = fmap snd
  $ find ((==a) . fst)
  $ pCells p

vertexAt :: Puzzle -> Vec2 -> Maybe Feature
vertexAt p a
  = fmap snd
  $ find ((==a) . fst)
  $ pVertices p

edgeAt :: Puzzle -> Vec2 -> Orientation -> Maybe Feature
edgeAt p a o
  = fmap (\(_, _, f) -> f)
  $ find (\(a', o', _) -> a == a' && o == o')
  $ pEdges p

edgesWhere :: Puzzle -> (Feature -> Bool) -> [(Vec2, Orientation)]
edgesWhere p f = map drop3 $ filter (f . trd) $ pEdges p
  where
    trd (_, _, x) = x
    drop3 (a, b, _) = (a, b)

verticesWhere :: Puzzle -> (Feature -> Bool) -> [Vec2]
verticesWhere p f = map fst $ filter (f . snd) $ pVertices p

hexRule :: Validator
hexRule puzzle soln = vertices && edges
  where
    vertices = all
      (flip elem $ verticesOfPath soln)
      (puzzle `verticesWhere` (==Hex))
    edges = all
      (flip elem soln)
      (puzzle `edgesWhere` (==Hex))

anyRepeated :: Eq a => [a] -> Bool
anyRepeated [] = False
anyRepeated (x:xs) = if x `elem` xs then True else anyRepeated xs
