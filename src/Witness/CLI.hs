{-# LANGUAGE LambdaCase #-}

module Witness.CLI where

import Witness.Core

import Data.List (intersperse)
import Data.Maybe (listToMaybe)
import System.IO

--  copied from:
--  http://hackage.haskell.org/package/cgi-3001.3.0.0/docs/Network-CGI-Protocol.html#v:maybeRead
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

--  Render a shape to a string:
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



renderPuzzleWithPath :: Puzzle -> Path -> String
renderPuzzleWithPath p soln 
  = (\p -> "\n" ++ p ++ edgeRow sr ++ "\n")
  $ concat
  $ map row [0..(pred sr)]
  where
    (sr, sc) = pDimensions p

    vertex :: Int -> Int -> String
    vertex r c = case (
        vertexAt p (r, c),
        (r, c) `elem` pSources p,
        (r, c) `elem` pSinks p,
        (r, c) `elem` verticesOfPath soln
      ) of
      (_, True, _, _)     -> "S"
      (_, _, True, _)     -> "K"
      (_, _, _, True)     -> "x"
      (Nothing, _, _, _)  -> "+"
      (Just Hex, _, _, _) -> "*"
      (Just Gap, _, _, _) -> " "

    edge :: Int -> Int -> Orientation -> String
    edge r c o = case (
        edgeAt p (r, c) o,
        o,
        ((r, c), o) `elem` soln
      ) of
      (_, Horizontal, True)     -> "="
      (_, Vertical, True)       -> "U"
      (Nothing, Horizontal, _)  -> "-"
      (Nothing, Vertical, _)    -> "|"
      (Just Hex, _, _)          -> "*"

    cell :: Int -> Int -> String
    cell r c = case cellAt p (r, c) of
      Nothing -> " "

    egdeGroup :: Int -> Int -> String
    egdeGroup r c = vertex r c ++ "--" ++ edge r c Horizontal ++ "--"

    edgeRow :: Int -> String
    edgeRow r = concat [ egdeGroup r c | c <- [0..(pred sc)] ] ++ vertex r sc

    cellGroup :: Int -> Int -> String
    cellGroup r c = edge r c Vertical ++ "  " ++ cell r c ++ "  "

    cellRow :: Int -> String
    cellRow r 
      = concat [ cellGroup r c | c <- [0..(pred sc)] ] ++ edge r sc Vertical

    row :: Int -> String
    row r = edgeRow r ++ "\n" ++ cellRow r ++ "\n"

selectSource :: Puzzle -> IO Vec2
selectSource p = let sources = pSources p in if length sources == 1
  then return $ head sources
  else do
    let iSources = zip sources [0..]
    putStrLn "Select a source:"
    flip mapM_ iSources
      $ \(s, i) -> putStrLn
        $ "\t[" ++ show i ++ "] for the source at " ++ show s
    ln <- getLine;
    case maybeRead ln of
      Nothing -> putStrLn "Bad imput" >> selectSource p
      Just n -> if n >= 0 && n < length sources
        then return $ sources !! n
        else putStrLn "Bad input" >> selectSource p

runPuzzle :: Puzzle -> IO ()
runPuzzle p = do
  source <- selectSource p
  path <- loopPuzzle p [] source
  putStrLn $ renderPuzzleWithPath p path
  if and $ map (\r -> r p path) (pRules p)
    then putStrLn "Success!"
    else putStrLn "Failure"

stepPuzzle :: Puzzle -> Path -> Vec2 -> IO (Path, Vec2)
stepPuzzle puzzle path v@(vr, vc) = do
  putStrLn $ renderPuzzleWithPath puzzle path
  move <- getMove puzzle path v

  let v' = case move of {
      North -> (pred vr, vc);
      South -> (succ vr, vc);
      West  -> (vr, pred vc);
      East  -> (vr, succ vc);
    }

  let newEdge = case move of {
      North -> ((pred vr, vc), Vertical);
      South -> ((vr, vc), Vertical);
      West  -> ((vr, pred vc), Horizontal);
      East  -> ((vr, vc), Horizontal);
    }

  return (newEdge:path, v')

loopPuzzle :: Puzzle -> Path -> Vec2 -> IO Path
loopPuzzle puzzle path vertex = do
  (path', vertex') <- stepPuzzle puzzle path vertex
  if vertex' `elem` pSinks puzzle
    then return path'
    else loopPuzzle puzzle path' vertex'

getMove :: Puzzle -> Path -> Vec2 -> IO Direction
getMove puzzle path v = do
  let moves = available v puzzle path
  putStr "Choose a direction (W/A/S/D)\n\tAvailable moves: "
  putStrLn
    $ concat
    $ intersperse " / "
    $ flip map moves 
    $ \case
      North -> "W"
      West  -> "A"
      South -> "S"
      East  -> "D"
  getWasd

getWasd :: IO Direction
getWasd = getChar >>= \case
  'w' -> return North
  'a' -> return West
  's' -> return South
  'd' -> return East
  _   -> getWasd