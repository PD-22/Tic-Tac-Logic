module Main where

import Control.Monad (unless)
import Helpers (gridIsValid, stringsToGrid)
import TTL (gridSolver)

main :: IO ()
main = do
  input <- getContents
  let grid = stringsToGrid (lines input)
  let solution = gridSolver grid
  let output = unlines solution
  if gridIsValid solution
    then putStrLn output
    else putStrLn "solution does not exist"