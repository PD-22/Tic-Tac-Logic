module Main where

import Control.Monad (unless)
import Helpers (gridIsValid, gridToStrings, inputIsValid, stringsToGrid)
import TTL (gridSolver)

main :: IO ()
main = do
  input <- getContents
  let grid = stringsToGrid (lines input)
  let solution = gridSolver grid
  let output = unlines (gridToStrings solution)
  if inputIsValid (lines input)
    then
      if gridIsValid solution
        then putStrLn output
        else putStrLn "solution does not exist"
    else putStrLn "input is not valid"