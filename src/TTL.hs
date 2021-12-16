-- Rules:
-- No Triples
-- No Dublicate Lines
-- Balanced X O amount

-- TODO:
-- use datatype?
-- add tests

module TTL (gridSolver, lineSolver) where

import Helpers (applyFunctions, doWhileChanges, transpose)
import Solvers (avoidTriple1, avoidTriple2)

lineSolver :: String -> String
lineSolver = doWhileChanges $ applyFunctions [avoidTriple1, avoidTriple2]

-- accept only even
gridSolver :: [String] -> [String]
gridSolver g = doWhileChanges callOnRowsCols g
  where
    callOnRowsCols g = transpose $ rowSolver $ transpose $ rowSolver g
    rowSolver = map lineSolver
