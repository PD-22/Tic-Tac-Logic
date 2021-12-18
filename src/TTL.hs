-- Rules:
-- No Triples
-- No Dublicate Lines
-- Balanced X O amount

module TTL (gridSolver, lineSolver) where

import Helpers (applyFunctions, doWhileChanges, replaceDot, transpose)
import Solvers (avoidTriple1, avoidTriple2, avoidTriple3, completeLine, avoidDuplication)

lineSolver :: String -> String
lineSolver =
  doWhileChanges $
    applyFunctions
      [ avoidTriple1,
        avoidTriple2,
        avoidTriple3,
        completeLine
      ]

gridSolver :: [String] -> [String]
gridSolver g = doWhileChanges graphSolvers g
  where
    graphSolvers = applyFunctions [lineSolvers, avoidDuplication]
    lineSolvers g = transpose $ rowSolver $ transpose $ rowSolver g
    rowSolver = map lineSolver
