-- Rules:
-- No Triples
-- No Dublicate Lines
-- Balanced X O amount

module TTL (gridSolver) where

import Helpers (applyFunctions, doOnRowsCols, doWhileChanges)
import Solvers
  ( advancedTech1,
    avoidDuplication,
    avoidTriple1,
    avoidTriple2,
    avoidTriple3,
    completeLine,
  )

lineSolver :: String -> String
lineSolver =
  doWhileChanges $
    applyFunctions
      [ avoidTriple1,
        avoidTriple2,
        avoidTriple3,
        completeLine,
        advancedTech1
      ]

gridSolver :: [String] -> [String]
gridSolver g = doWhileChanges graphSolvers g
  where
    graphSolvers = applyFunctions [lineSolvers, avoidDuplication]
    lineSolvers g = doOnRowsCols rowSolver g
    rowSolver = map lineSolver
