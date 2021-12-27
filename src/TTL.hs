module TTL (gridSolver, Cell, Line, Grid) where

import Helpers
  ( Cell (..),
    Grid,
    Line,
    applyWhileChanges,
    countDot2,
    doOnRowsCols,
    gridIsFilled,
    gridIsValid,
    replaceFirst2,
  )
import Solvers
  ( advancedTech1,
    advancedTech2,
    avoidDuplication,
    avoidTriple1,
    avoidTriple2,
    avoidTriple3,
    completeLine,
  )

-- Rules:
-- No Triples
-- No Dublicate Lines
-- Balanced X O amount

lineSolver :: Line -> Line
lineSolver =
  applyWhileChanges
    [ avoidTriple1,
      avoidTriple2,
      avoidTriple3,
      completeLine,
      advancedTech1
    ]

techSolver :: Grid -> Grid
techSolver = applyWhileChanges [gridLineSolver, avoidDuplication, advancedTech2]
  where
    gridLineSolver = doOnRowsCols (map lineSolver)

gridSolver :: Grid -> Grid
gridSolver g = gridSolverRec g True

gridSolverRec :: Grid -> Bool -> Grid
gridSolverRec og b =
  if gridIsFilled g
    then g
    else bruteForce g (not b)
  where
    g = techSolver og

bruteForce :: Grid -> Bool -> Grid
bruteForce g b
  | gridIsValid way1 && gridIsFilled way1 = way1
  | gridIsValid way2 && gridIsFilled way2 = way2
  | otherwise = g
  where
    (way1, way2) = if b then (xWay, oWay) else (oWay, xWay)
    xWay = gridSolverRec (replaceFirst2 E X g) b
    oWay = gridSolverRec (replaceFirst2 E O g) b