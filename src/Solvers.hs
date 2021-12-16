module Solvers where

avoidTriple1 :: String -> String
avoidTriple1 [] = []
avoidTriple1 [a] = [a]
avoidTriple1 [a, b] = [a, b]
avoidTriple1 ('.' : 'x' : 'x' : bs) = 'o' : avoidTriple1 ('x' : 'x' : bs)
avoidTriple1 ('x' : 'x' : '.' : bs) = 'x' : 'x' : avoidTriple1 ('o' : bs)
avoidTriple1 ('.' : 'o' : 'o' : bs) = 'x' : avoidTriple1 ('o' : 'o' : bs)
avoidTriple1 ('o' : 'o' : '.' : bs) = 'o' : 'o' : avoidTriple1 ('x' : bs)
avoidTriple1 (c : cs) = c : avoidTriple1 cs

-- theres no need to check for x/o balance
avoidTriple2 :: String -> String
avoidTriple2 [] = []
avoidTriple2 [a] = [a]
avoidTriple2 [a, b] = [a, b]
avoidTriple2 ('x' : '.' : 'x' : bs) = 'x' : 'o' : avoidTriple2 ('x' : bs)
avoidTriple2 ('o' : '.' : 'o' : bs) = 'o' : 'x' : avoidTriple2 ('o' : bs)
avoidTriple2 (c : cs) = c : avoidTriple2 cs
