module Main where

import Control.Monad (unless)
import TTL (lineSolver)

main :: IO ()
main = do
  input <- getLine
  putStr $ lineSolver input

-- input <- readFile "src/gen_res.txt"
-- let inputLines = filter (not . null) (lines input)
-- let output = unlines $ map lineSolver inputLines
-- putStrLn output
-- writeFile "src/ttl_res.txt" output
