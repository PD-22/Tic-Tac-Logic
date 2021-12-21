module Solvers where

import GHC.ST (STret)
import Helpers
  ( combsXO,
    countRemainXO,
    doWhileChanges,
    hasTriple,
    listCombs,
    mergeCombs,
    replace,
    replaceDot,
    spreadOnDots,
    transpose,
  )

-- TODO: might need to generalize some solvers
-- for example in avoidTriple3 use one X and more than one O cases

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

-- if only one valid comb spread that
-- else spread merged inverted invalid combs

avoidTriple3 :: String -> String
avoidTriple3 [] = []
avoidTriple3 l
  | charToGuess == 'N' = l
  | otherwise = spreadOnDots combToSpread l
  where
    combToSpread
      | length validCombs == 1 = head validCombs
      | null inValidCombs = emptyComb
      | otherwise = mergeCombs $ invertInvalid inValidCombs
    validCombs = map snd $ filter fst validCombMap
    inValidCombs = map snd $ filter (not . fst) validCombMap
    invertInvalid = map (replace charToGuess otherChar . replace otherChar '.')
    emptyComb = replicate (rx + ro) '.'
    (rx, ro) = countRemainXO l
    validCombMap = map (\c -> (isCombValid c, c)) combs
    (charToGuess, otherChar)
      | rx == 1 && ro == 2 = ('x', 'o')
      | ro == 1 && rx == 2 = ('o', 'x')
      | otherwise = ('N', 'N')
    combs = combsXO rx ro
    isCombValid comb = not $ hasTriple (spreadOnDots comb l)

completeLine :: String -> String
completeLine l
  | x == 1 && o == 0 = replaceDot 'x' l
  | x == 0 && o == 1 = replaceDot 'o' l
  | otherwise = l
  where
    (x, o) = countRemainXO l

-- -- -- -- -- -- -- -- -- -- -- --

-- TODO
-- ? test avoidDuplication

avoidDuplication :: [String] -> [String]
avoidDuplication = doWhileChanges avoidDupHelp1

avoidDupHelp1 :: [String] -> [String]
avoidDupHelp1 g = transpose $ avoidDupHelp2 $ transpose $ avoidDupHelp2 g

avoidDupHelp2 :: [String] -> [String]
avoidDupHelp2 g = solveRows g
  where
    solveRows = map linesMap
    linesMap l =
      let remainDots = countRemainDot l
          otherLines = filter (/= l) g
       in if remainDots == 2
            then tryDupSolve l otherLines
            else l

tryDupSolve :: String -> [String] -> String
tryDupSolve ls ol =
  if null linesToCompare
    then ls
    else solveDup ls lineToCompare
  where
    linesToCompare = filter (couldBeIdentical ls) ol
    lineToCompare = head linesToCompare

couldBeIdentical :: [Char] -> String -> Bool
couldBeIdentical ls lc = some (== lc) allVariants
  where
    allVariants = map (`spreadOnDots` ls) spreads
    spreads = listCombs (replicate rx 'x' ++ replicate ro 'o')
    (rx, ro) = countRemainXO ls

some :: Foldable t => (a -> Bool) -> t a -> Bool
some f = foldr ((||) . f) False

solveDup :: String -> String -> String
solveDup lToSolve lToCompare = spreadOnDots correctSpread lToSolve
  where
    reverseChar c
      | c == 'x' = 'o'
      | c == 'o' = 'x'
      | otherwise = '.'
    opposite s = map reverseChar s
    correctSpread = opposite (head (map fst invalidSpreads))
    invalidSpreads = filter (\(sprd, res) -> res == lToCompare) spreadResMap
    spreadResMap = map (\sprd -> (sprd, spreadOnDots sprd lToSolve)) possibleSpreads
    possibleSpreads = listCombs (replicate rx 'x' ++ replicate ro 'o')
    (rx, ro) = countRemainXO lToSolve

countRemainDot :: String -> Int
countRemainDot l = rx + ro
  where
    (rx, ro) = countRemainXO l
