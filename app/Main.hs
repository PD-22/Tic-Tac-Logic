module Main where

import Control.Monad (unless)
import Helpers (gridIsValid)
import TTL (gridSolver)

main :: IO ()
main = do
  input <- getContents
  let grid = lines input
  let solution = gridSolver grid
  let output = unlines solution
  putStrLn ("solution is " ++ if gridIsValid solution then "valid" else "not valid")
  putStrLn output