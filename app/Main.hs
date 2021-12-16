module Main where

import Control.Monad (unless)
import TTL (gridSolver, lineSolver)

main :: IO ()
main = do
  input <- getContents
  putStr (unlines (gridSolver (lines input)))

-- input <- readFile "src/gen_res.txt"
-- let inputLines = filter (not . null) (lines input)
-- let output = unlines $ map lineSolver inputLines
-- putStrLn output
-- writeFile "src/ttl_res.txt" output
