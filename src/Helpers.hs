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
