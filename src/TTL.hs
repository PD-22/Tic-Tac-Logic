-- Rules:
-- No Triples
-- No Dublicate Lines
-- Balanced X O amount

module TTL (gridSolver) where

import Helpers (applyFunctions, doOnRowsCols, doWhileChanges)
import Solvers
  ( advancedTech1,
    advancedTech2,
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

gridSolvers :: [[String] -> [String]]
gridSolvers = [lineSolvers, avoidDuplication, advancedTech2]
  where
    lineSolvers = doOnRowsCols rowSolver
    rowSolver = map lineSolver

gridSolver :: [String] -> [String]
gridSolver = doWhileChanges (applyFunctions gridSolvers)
