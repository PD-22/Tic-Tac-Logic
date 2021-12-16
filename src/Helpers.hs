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

countXO :: (Int, Int) -> Char -> (Int, Int)
countXO (x, o) char =
  case char of
    'x' -> (x + 1, o)
    'o' -> (x, o + 1)
    _ -> (x, o)
