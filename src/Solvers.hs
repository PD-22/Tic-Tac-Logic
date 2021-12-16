module Solvers where

import Helpers (countRemainXO)

avoidTriple1 :: String -> String
avoidTriple1 [] = []
avoidTriple1 [a] = [a]
avoidTriple1 [a, b] = [a, b]
avoidTriple1 ('.' : 'x' : 'x' : bs) = 'o' : avoidTriple1 ('x' : 'x' : bs)
avoidTriple1 ('x' : 'x' : '.' : bs) = 'x' : 'x' : avoidTriple1 ('o' : bs)
avoidTriple1 ('.' : 'o' : 'o' : bs) = 'x' : avoidTriple1 ('o' : 'o' : bs)
avoidTriple1 ('o' : 'o' : '.' : bs) = 'o' : 'o' : avoidTriple1 ('x' : bs)
avoidTriple1 (c : cs) = c : avoidTriple1 cs

avoidTriple2 :: String -> String
avoidTriple2 [] = []
avoidTriple2 [a] = [a]
avoidTriple2 [a, b] = [a, b]
avoidTriple2 ('x' : '.' : 'x' : bs) = 'x' : 'o' : avoidTriple2 ('x' : bs)
avoidTriple2 ('o' : '.' : 'o' : bs) = 'o' : 'x' : avoidTriple2 ('o' : bs)
avoidTriple2 (c : cs) = c : avoidTriple2 cs

-- .x..oxxo  1x 2o
-- more examples
-- avoidTriple3 :: String -> String
-- avoidTriple3 [] = []
-- avoidTriple3 [a] = [a]
-- avoidTriple3 [a, b] = [a, b]
-- avoidTriple3 l = if (x == 1) && (o == 2) then place 'x' else l
--   where
--     (x, o) = countRemainXO l

completeLine :: String -> String
completeLine l
  | (x == 1) && (o == 0) = replaceDot 'x' l
  | (x == 0) && (o == 1) = replaceDot 'o' l
  | otherwise = l
  where
    replaceDot r = map (\c -> if c == '.' then r else c)
    (x, o) = countRemainXO l
