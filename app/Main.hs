module Main where

import Control.Monad (unless)
import TTL (gridSolver)

main :: IO ()
main = do
  input <- getContents
  printGirdSolve gridSolver input

printGirdSolve :: ([String] -> [String]) -> String -> IO ()
printGirdSolve solver input = putStr (unlines (solver (lines input)))