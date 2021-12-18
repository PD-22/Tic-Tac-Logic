module Main where

import Control.Monad (unless)
import TTL (gridSolver)

main :: IO ()
main = do
  input <- getContents
  putStr (unlines (gridSolver (lines input)))
