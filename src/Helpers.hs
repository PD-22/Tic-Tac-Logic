module Helpers where

import Data.List (sort)

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

mergeCombs :: [String] -> String
mergeCombs = foldl mergeCombsHelp1 []

mergeCombsHelp1 :: String -> String -> String
mergeCombsHelp1 [] b = b
mergeCombsHelp1 a [] = a
mergeCombsHelp1 (ah : at) (bh : bt) = mergeCombsHelp2 ah bh : mergeCombsHelp1 at bt

mergeCombsHelp2 :: Char -> Char -> Char
mergeCombsHelp2 '.' a = a
mergeCombsHelp2 a '.' = a
mergeCombsHelp2 a b = if a == b then a else '.'

combsXO :: Int -> Int -> [String]
combsXO xn on = listCombs (replicate xn 'x' ++ replicate on 'o')

listCombs :: String -> [String]
listCombs [c] = [[c]]
listCombs l = rmdups $ concatMap mapHelp l
  where
    mapHelp c = map (c :) (listCombs (removeFirst c l))

removeFirst :: Eq t => t -> [t] -> [t]
removeFirst _ [] = []
removeFirst c (sh : st) = if c == sh then st else sh : removeFirst c st

rmdups :: (Ord a) => [a] -> [a]
rmdups = rmdupsSorted . sort

rmdupsSorted :: Eq a => [a] -> [a]
rmdupsSorted [] = []
rmdupsSorted (lh : lt) = lh : rmdupsSorted (rmTill lh lt)

rmTill :: Eq t => t -> [t] -> [t]
rmTill c [] = []
rmTill c l@(lh : lt) = if lh == c then rmTill c lt else l
