module TTL (gridSolver) where

import Helpers
  ( applyWhileChanges,
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

-- TODO: clean imports after cleaning TTL
-- Rules:
-- No Triples
-- No Dublicate Lines
-- Balanced X O amount

lineSolver :: String -> String
lineSolver =
  applyWhileChanges
    [ avoidTriple1,
      avoidTriple2,
      avoidTriple3,
      completeLine,
      advancedTech1
    ]

techSolver :: [String] -> [String]
techSolver = applyWhileChanges [gridLineSolver, avoidDuplication, advancedTech2]
  where
    gridLineSolver = doOnRowsCols (map lineSolver)

gridSolver :: [String] -> [String]
gridSolver og = if isFilled g then g else bruteForce
  where
    g = techSolver og
    bruteForce
      | gridIsValid xWay = xWay
      | gridIsValid oWay = oWay
      | otherwise = g
    xWay = gridSolver (replaceFirst2 '.' 'x' g)
    oWay = gridSolver (replaceFirst2 '.' 'o' g)
    isFilled g = countDot2 g == 0
