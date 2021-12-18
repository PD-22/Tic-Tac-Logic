module Helpers where

doWhileChanges :: Eq t => (t -> t) -> t -> t
doWhileChanges f oldValue =
  if newValue == oldValue
    then oldValue
    else doWhileChanges f newValue
  where
    newValue = f oldValue

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose m = map head m : transpose (map tail m)

applyFunctions :: [t -> t] -> t -> t
applyFunctions fs v = foldl (\v f -> f v) v fs

countRemainXO :: String -> (Int, Int)
countRemainXO l = (shouldBe - xNum, shouldBe - oNum)
  where
    shouldBe = length l `div` 2
    (xNum, oNum) = foldl countXO (0, 0) l
    countXO (x, o) char = case char of
      'x' -> (x + 1, o)
      'o' -> (x, o + 1)
      _ -> (x, o)

replaceDot :: Char -> String -> String
replaceDot = replace '.'

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\c -> if c == a then b else c)

spreadOnDots :: String -> String -> String
spreadOnDots [] l = l
spreadOnDots _ [] = []
spreadOnDots rs@(rh : rt) (lh : lt) =
  if lh == '.'
    then rh : spreadOnDots rt lt
    else lh : spreadOnDots rs lt

hasTriple :: String -> Bool
hasTriple [] = False
hasTriple [a] = False
hasTriple [a, b] = False
hasTriple ('x' : 'x' : 'x' : _) = True
hasTriple ('o' : 'o' : 'o' : _) = True
hasTriple (c : cs) = hasTriple cs
