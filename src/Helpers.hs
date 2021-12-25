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

remainXO :: String -> (Int, Int)
remainXO l = (shouldBe - xNum, shouldBe - oNum)
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

some :: Foldable t => (a -> Bool) -> t a -> Bool
some f = foldr ((||) . f) False

fillVariants :: String -> ([String], [String])
fillVariants l = filter2 isValid (spreadCombs l)
  where
    isValid c =
      let spreadResult = spreadOnDots c l
       in not (hasTriple spreadResult)

fillVariants2 :: String -> [String] -> ([String], [String])
fillVariants2 l g = filter2 isValid (spreadCombs l)
  where
    isValid c =
      let spreadResult = spreadOnDots c l
       in not (hasTriple spreadResult || makesDupl c)
    makesDupl c = some (\ol -> linesAreSame ol (spreadOnDots c l)) ols
    ols = filter (/= l) g

-- returns both filtered and unfiltered elements as tuple
filter2 :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
filter2 f = foldr (\cur (a, b) -> if f cur then (cur : a, b) else (a, cur : b)) ([], [])

spreadCombs :: String -> [String]
spreadCombs l = let (rx, ro) = remainXO l in combsXO rx ro

-- finds correct using char positions (spread) that only invalids have
{- for example:
valids = ["xxoxox","xxxoox","xxxoxo"]
invalids = ["ooxxxx","oxoxxx","oxxoxx","oxxxox","oxxxxo","xooxxx",
  "xoxoxx","xoxxox","xoxxxo","xxooxx","xxoxxo","xxxxoo"]
listCommons valids = ["x","x","ox","ox","ox","ox"]
listCommons inValids = ["ox","ox","ox","ox","ox","ox"]
result = [['o'],['o'],[],[],[],[]]
spread = "xx...." -}
-- TODO:
-- clean code duplication in onlyValidSpread and onlyValidSpread2
onlyValidSpread :: String -> String
onlyValidSpread l = map mapHelp onlyInvalidsHave
  where
    onlyInvalidsHave = zipWith oneHasOtherNot (listCommons is) (listCommons vs)
    (vs, is) = fillVariants l
    mapHelp cs = if length cs == 1 then reverseChar (head cs) else '.'
    reverseChar c
      | c == 'x' = 'o'
      | c == 'o' = 'x'
      | otherwise = '.'

onlyValidSpread2 :: String -> [String] -> String
onlyValidSpread2 l g = map mapHelp onlyInvalidsHave
  where
    onlyInvalidsHave = zipWith oneHasOtherNot (listCommons is) (listCommons vs)
    (vs, is) = fillVariants2 l g
    mapHelp cs = if length cs == 1 then reverseChar (head cs) else '.'
    reverseChar c
      | c == 'x' = 'o'
      | c == 'o' = 'x'
      | otherwise = '.'

-- finds elements that are in first list but not in second
oneHasOtherNot :: (Foldable t, Eq a) => [a] -> t a -> [a]
oneHasOtherNot l1 l2 = filter (\c -> not $ some (== c) l2) l1

-- gives info for what characters appear at each position
listCommons :: [String] -> [String]
listCommons [] = []
listCommons [l] = map (: []) l
listCommons (lh : lt) = foldl foldHelp start lt
  where
    start = map (: []) lh
    foldHelp acc cur = zipWith zipHelp acc cur
    zipHelp a b = rmdups (b : a)

mergeCombs :: [String] -> String
mergeCombs = replace 'N' '.' . foldl mergeCombsHelp1 []

mergeCombsHelp1 :: String -> String -> String
mergeCombsHelp1 [] b = b
mergeCombsHelp1 a [] = a
mergeCombsHelp1 (ah : at) (bh : bt) = mergeCombsHelp2 ah bh : mergeCombsHelp1 at bt

mergeCombsHelp2 :: Char -> Char -> Char
mergeCombsHelp2 '.' a = a
mergeCombsHelp2 a '.' = a
mergeCombsHelp2 a b = if a == b then a else 'N'

combsXO :: Int -> Int -> [String]
combsXO xn on = rearrangeCombs (replicate xn 'x' ++ replicate on 'o')

rearrangeCombs :: String -> [String]
rearrangeCombs [c] = [[c]]
rearrangeCombs l = rmdups $ concatMap mapHelp l
  where
    mapHelp c = map (c :) (rearrangeCombs (removeFirst c l))

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

-- ? maybe merge
countRemainDot :: String -> Int
countRemainDot l = rx + ro
  where
    (rx, ro) = remainXO l

countDot :: String -> Int
countDot = foldl (\a c -> a + if c == '.' then 1 else 0) 0

doOnRowsCols :: ([String] -> [String]) -> [String] -> [String]
doOnRowsCols f g = transpose $ f $ transpose $ f g

replaceFirst :: Eq t => t -> t -> [t] -> [t]
replaceFirst _ _ [] = []
replaceFirst c1 c2 (a : as) = if a == c1 then c2 : as else a : replaceFirst c1 c2 as

-- True if they have same x and o positions
linesAreSame :: [Char] -> [Char] -> Bool
linesAreSame l1 l2 = and sameCharZip
  where
    sameCharZip = zipWith charZipper l1 l2
    charZipper a b = not (a == '.' || b == '.') && (a == b)

-- takes a line and grid that has the line
hasDupl :: String -> [String] -> Bool
hasDupl l g = gridHasLine && lineHasDupl
  where
    gridHasLine = some (linesAreSame l) g
    lineHasDupl = duplCount > 1
    duplCount = foldl countFolder 0 g
    countFolder acc cur = acc + (if linesAreSame cur l then 1 else 0)

hasBalancedXO :: String -> Bool
hasBalancedXO l = (rx <= shouldBe) && (ro <= shouldBe)
  where
    (rx, ro) = remainXO l
    shouldBe = length l `div` 2

countDot2 :: [String] -> Int
countDot2 = foldl (\a b -> a + countDot b) 0

-- same as replaceFirst but operates on whole grid
replaceFirst2 :: Char -> Char -> [String] -> [String]
replaceFirst2 _ _ [] = []
replaceFirst2 c1 c2 (l : ls) = if didReplace then nl : ls else l : replaceFirst2 c1 c2 ls
  where
    didReplace = nl /= l
    nl = replaceFirst c1 c2 l

gridIsValid :: [String] -> Bool
gridIsValid g = rowsAreValid g && rowsAreValid (transpose g)
  where
    rowsAreValid rs = foldl (\b l -> b && lineIsValid l rs) True rs
    lineIsValid l rs = not (hasTriple l || hasDupl l rs) && hasBalancedXO l

applyWhileChanges :: Eq t => [t -> t] -> t -> t
applyWhileChanges fs = doWhileChanges (applyFunctions fs)
