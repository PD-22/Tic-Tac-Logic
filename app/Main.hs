module Main where

import Control.Monad (unless)
import Helpers (gridIsValid, gridToStrings, inputIsValid, stringsToGrid, parseRowCol)
import TTL (gridSolver)

main :: IO ()
main = do
  input <- getContents
  let inputLines = lines input

  let (rows, cols) = parseRowCol (head inputLines)
  let grid = stringsToGrid (tail inputLines)

  let solution = gridSolver grid
  let output = unlines (gridToStrings solution)
  
  if inputIsValid inputLines
    then
      if gridIsValid solution
        then putStrLn output
        else putStrLn "solution does not exist"
    else putStrLn "input is not valid"
