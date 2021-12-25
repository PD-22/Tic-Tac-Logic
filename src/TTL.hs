module TTL (gridSolver, Cell, Line, Grid) where

import Helpers
  ( Cell (..),
    Grid,
    Line,
    applyWhileChanges,
    countDot2,
    doOnRowsCols,
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
gridSolver og = if isFilled g then g else bruteForce
  where
    g = techSolver og
    bruteForce
      | gridIsValid xWay = xWay
      | gridIsValid oWay = oWay
      | otherwise = g
    xWay = gridSolver (replaceFirst2 E X g)
    oWay = gridSolver (replaceFirst2 E O g)
    isFilled g = countDot2 g == 0
