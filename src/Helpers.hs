module Helpers where

import Data.List (sort)

data Cell = X | O | E | N deriving (Show, Eq, Ord)

type Line = [Cell]

type Grid = [Line]

-- check if puzzle input is valid
inputIsValid :: [String] -> Bool
inputIsValid inputLines =
  linesHaveSameLength
    && even cols
    && even rows
    && hasOnlyXODot
    && inputGridIsValid
    && rowColMatches
  where
    rowColLines = head inputLines
    g = tail inputLines
    height = length g
    width = length (head g)
    hasOnlyXODot = every (every (\c -> some (== c) "XOxo.")) g
    linesHaveSameLength = every (\l -> length l == length (head g)) g
    inputGridIsValid = gridIsValid (stringsToGrid g)
    (rows, cols) = parseRowCol rowColLines
    rowColMatches = height == rows && width == cols

-- convert input grid to a grid type
stringsToGrid :: [String] -> Grid
stringsToGrid = map lineMapper
  where
    lineMapper str = map charMapper str
    charMapper char
      | char == 'X' || char == 'x' = X
      | char == 'O' || char == 'o' = O
      | char == '.' = E
      | otherwise = N

-- convert grid to string (e.g. for printing)
gridToStrings :: Grid -> [String]
gridToStrings = map lineMapper
  where
    lineMapper line = map charMapper line
    charMapper cell
      | cell == X = 'X'
      | cell == O = 'O'
      | cell == E = '.'
      | otherwise = ' '

-- feed value to the function while producing different result
doWhileChanges :: Eq t => (t -> t) -> t -> t
doWhileChanges f oldValue =
  if newValue == oldValue
    then oldValue
    else doWhileChanges f newValue
  where
    newValue = f oldValue

-- apply all functions to the value by chaining them
applyFunctions :: [t -> t] -> t -> t
applyFunctions fs v = foldl (\v f -> f v) v fs

-- merge of doWhileChanges and applyFunctions
applyWhileChanges :: Eq t => [t -> t] -> t -> t
applyWhileChanges fs = doWhileChanges (applyFunctions fs)

-- transpose grid as matrix
transpose :: Grid -> Grid
transpose [] = []
transpose ([] : _) = []
transpose m = map head m : transpose (map tail m)

-- return remain Xs and Ox to solve the line
remainXO :: Line -> (Int, Int)
remainXO l = (shouldBe - xNum, shouldBe - oNum)
  where
    shouldBe = length l `div` 2
    (xNum, oNum) = countXO l

countXO :: Line -> (Int, Int)
countXO l = foldl countHelp (0, 0) l
  where
    countHelp (x, o) char = case char of
      X -> (x + 1, o)
      O -> (x, o + 1)
      _ -> (x, o)

-- replace all dots with given cell
replaceDot :: Cell -> Line -> Line
replaceDot = replace E

-- replace all As with Bs
replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\c -> if c == a then b else c)

-- spread comb on list dots
-- e.g. xoo ..x.xo -> xoxoxo
spreadOnDots :: Line -> Line -> Line
spreadOnDots [] l = l
spreadOnDots _ [] = []
spreadOnDots rs@(rh : rt) (lh : lt) =
  if lh == E
    then rh : spreadOnDots rt lt
    else lh : spreadOnDots rs lt

-- check if line has a triple
hasTriple :: Line -> Bool
hasTriple [] = False
hasTriple [a] = False
hasTriple [a, b] = False
hasTriple (X : X : X : _) = True
hasTriple (O : O : O : _) = True
hasTriple (c : cs) = hasTriple cs

-- check if some elt satisfies the function
some :: Foldable t => (a -> Bool) -> t a -> Bool
some f = foldr ((||) . f) False

-- check if all elts satisfy the function
every :: Foldable t => (a -> Bool) -> t a -> Bool
every f = foldr ((&&) . f) True

-- return valid and invalid possible combs to solve the line
computeFillVariants :: Line -> (Line -> Bool) -> ([[Cell]], [[Cell]])
computeFillVariants l lineIsValid = filter2 isValid (spreadCombs l)
  where
    isValid c =
      let spreadResult = spreadOnDots c l
       in lineIsValid spreadResult

-- return possible combs by checking if comb makes a triple
fillVariants :: Line -> ([[Cell]], [[Cell]])
fillVariants l = computeFillVariants l lineIsValid
  where
    lineIsValid = not . hasTriple

-- return possible combs by checking if comb makes a triple or a duplicate
fillVariants2 :: Line -> Grid -> ([[Cell]], [[Cell]])
fillVariants2 l g = computeFillVariants l lineIsValid
  where
    lineIsValid l = not (hasTriple l || makesDupl l)
    makesDupl l = some (haveSameXOs l) ols
    ols = filter (/= l) g

-- return both filtered and unfiltered elements as tuple
filter2 :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
filter2 f = foldr (\cur (a, b) -> if f cur then (cur : a, b) else (a, cur : b)) ([], [])

-- get all possible combs to spread on line
spreadCombs :: Line -> [Line]
spreadCombs l = let (rx, ro) = remainXO l in combsXO rx ro

-- find correct spread by inverting chars that only invalids have at that position
computeValidSpread :: ([Line], [Line]) -> [Cell]
computeValidSpread getValidsInvalids = map mapHelp onlyInvalidsHave
  where
    onlyInvalidsHave = zipWith oneHasOtherNot (listCommons is) (listCommons vs)
    (vs, is) = getValidsInvalids
    mapHelp cs = if length cs == 1 then reverseChar (head cs) else E
    reverseChar c
      | c == X = O
      | c == O = X
      | otherwise = E

-- return only valid spread for line
onlyValidSpread :: Line -> Line
onlyValidSpread l = computeValidSpread (fillVariants l)

-- return only valid spread for grid
onlyValidSpread2 :: Line -> Grid -> Line
onlyValidSpread2 l g = computeValidSpread (fillVariants2 l g)

-- find elements that are in first list but not in second
oneHasOtherNot :: (Foldable t, Eq a) => [a] -> t a -> [a]
oneHasOtherNot l1 l2 = filter (\c -> every (/= c) l2) l1

-- give info for what characters appear at each position
-- e.g. abb aab -> [[a],[b,a],[b]]
listCommons :: [Line] -> [Line]
listCommons [] = []
listCommons [l] = map (: []) l
listCommons (lh : lt) = foldl foldHelp start lt
  where
    start = map (: []) lh
    foldHelp acc cur = zipWith zipHelp acc cur
    zipHelp a b = rmdups (b : a)

-- same as mergeTwoCombs but merge all combs together
mergeCombs :: [Line] -> Line
mergeCombs = replace N E . foldl mergeTwoCombs []

-- merge two combs to get a comb that will have same spread effect
mergeTwoCombs :: Line -> Line -> Line
mergeTwoCombs [] b = b
mergeTwoCombs a [] = a
mergeTwoCombs (ah : at) (bh : bt) = mergeCombsCell ah bh : mergeTwoCombs at bt

-- merge combs helper
mergeCombsCell :: Cell -> Cell -> Cell
mergeCombsCell E a = a
mergeCombsCell a E = a
mergeCombsCell a b = if a == b then a else N

-- returns all possible spread combs for given remaining Xs and Os
combsXO :: Int -> Int -> [Line]
combsXO xn on = rearrangeCombs (replicate xn X ++ replicate on O)

-- combsXO helper for making all possible combinations of a list
rearrangeCombs :: Ord a => [a] -> [[a]]
rearrangeCombs [c] = [[c]]
rearrangeCombs l = rmdups $ concatMap mapHelp l
  where
    mapHelp c = map (c :) (rearrangeCombs (removeFirst c l))

-- removest first elt that equals to given elt
removeFirst :: Eq t => t -> [t] -> [t]
removeFirst _ [] = []
removeFirst c (sh : st) = if c == sh then st else sh : removeFirst c st

-- removes duplicate elts of list by sorting it first
rmdups :: (Ord a) => [a] -> [a]
rmdups = rmdupsSorted . sort

-- rmdups helper that only removes dups from sorted list
rmdupsSorted :: Eq a => [a] -> [a]
rmdupsSorted [] = []
rmdupsSorted (lh : lt) = lh : rmdupsSorted (rmTill lh lt)

-- removes elt untill it equals to the given elt
rmTill :: Eq t => t -> [t] -> [t]
rmTill c [] = []
rmTill c l@(lh : lt) = if lh == c then rmTill c lt else l

-- count empty cells in a line
countDot :: Line -> Int
countDot = foldl (\a c -> a + if c == E then 1 else 0) 0

-- count empty cells in a grid
countDot2 :: Grid -> Int
countDot2 = foldl (\a b -> a + countDot b) 0

-- apply function to rows and cols of the grid by transposing it twice
doOnRowsCols :: (Grid -> Grid) -> Grid -> Grid
doOnRowsCols f g = transpose $ f $ transpose $ f g

-- replace first occurrence of elt by another elt
replaceFirst :: Eq t => t -> t -> [t] -> [t]
replaceFirst _ _ [] = []
replaceFirst c1 c2 (a : as) = if a == c1 then c2 : as else a : replaceFirst c1 c2 as

-- same as replaceFirst but operates on whole grid
replaceFirst2 :: Cell -> Cell -> Grid -> Grid
replaceFirst2 _ _ [] = []
replaceFirst2 c1 c2 (l : ls) = if didReplace then nl : ls else l : replaceFirst2 c1 c2 ls
  where
    didReplace = nl /= l
    nl = replaceFirst c1 c2 l

-- check if lines have same x and o positions
haveSameXOs :: Line -> Line -> Bool
haveSameXOs l1 l2 = and sameCharZip
  where
    sameCharZip = zipWith charZipper l1 l2
    charZipper a b = not (a == E || b == E) && (a == b)

-- check if line is a duplicate in its graph
hasDupl :: Line -> Grid -> Bool
hasDupl l g = gridHasLine && lineHasDupl
  where
    gridHasLine = some (haveSameXOs l) g
    lineHasDupl = duplCount > 1
    duplCount = foldl countFolder 0 g
    countFolder acc cur = acc + (if haveSameXOs cur l then 1 else 0)

-- check if the number of X or O has exceeded its limit
hasBalancedXO :: Line -> Bool
hasBalancedXO l = (x <= maxNum) && (o <= maxNum)
  where
    (x, o) = countXO l
    maxNum = length l `div` 2

-- check if the grid lines form a triple, duplicate or unbalanced amount of X and Os
gridIsValid :: Grid -> Bool
gridIsValid g = rowsAreValid g && rowsAreValid (transpose g)
  where
    rowsAreValid rs = foldl (\b l -> b && lineIsValid l rs) True rs
    lineIsValid l rs = not (hasTriple l || hasDupl l rs) && hasBalancedXO l

-- check if grid does not have empty cells left
gridIsFilled :: Grid -> Bool
gridIsFilled g = countDot2 g == 0

parseRowCol :: String -> (Int, Int)
parseRowCol str = (rows, cols)
  where
    [rows, cols] = map (\s -> (read s :: Int)) (words str)
