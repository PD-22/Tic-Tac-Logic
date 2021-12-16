module Main where

import Control.Monad (unless)
import TTL (gridSolver, lineSolver)

main :: IO ()
main = do
  input <- getContents
  putStr (unlines (gridSolver (lines input)))
