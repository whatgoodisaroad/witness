import Witness.Core
import Witness.CLI

testPuzzle :: Puzzle
testPuzzle = Grid {
    pDimensions = (4,4),
    pVertices = [],
    pEdges = [
        ((0, 0), Horizontal, Hex),
        ((0, 0), Vertical, Hex)
      ],
    pCells = [],
    pSources = [(0,2), (0,3)],
    pSinks = [(4,3)],
    pRules = [ hexRule ]
  }

main :: IO ()
main = runPuzzle testPuzzle
