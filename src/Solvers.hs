module Solvers where

import GHC.ST (STret)
import Helpers
  ( combsXO,
    doWhileChanges,
    fillVariants,
    hasTriple,
    listCommons,
    mergeCombs,
    oneHasOtherNot,
    rearrangeCombs,
    remainXO,
    replace,
    replaceDot,
    some,
    spreadOnDots,
    transpose,
  )

-- TODO
-- generalize some solvers
-- use fillVariants

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

-- TODO: use added helper to get (valids, invalids)
-- ? How does it work
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
    (rx, ro) = remainXO l
    validCombMap = map (\c -> (isCombValid c, c)) combs
    (charToGuess, otherChar)
      | rx == 1 && ro > 1 = ('x', 'o')
      | ro == 1 && rx > 1 = ('o', 'x')
      | otherwise = ('N', 'N')
    combs = combsXO rx ro
    isCombValid comb = not $ hasTriple (spreadOnDots comb l)

completeLine :: String -> String
completeLine l
  | o == 0 && x > 0 = replaceDot 'x' l
  | x == 0 && o > 0 = replaceDot 'o' l
  | otherwise = l
  where
    (x, o) = remainXO l

--------------------------------

-- TODO
-- clean thsi section
-- ? test avoidDuplication
-- ? clean stuff, make compact
-- ? generalize

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
    linesToCompare = filter (couldBeDuplicate ls) ol
    lineToCompare = head linesToCompare

couldBeDuplicate :: [Char] -> String -> Bool
couldBeDuplicate ls lc = some (== lc) allVariants
  where
    allVariants = map (`spreadOnDots` ls) spreads
    spreads = rearrangeCombs (replicate rx 'x' ++ replicate ro 'o')
    (rx, ro) = remainXO ls

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
    possibleSpreads = rearrangeCombs (replicate rx 'x' ++ replicate ro 'o')
    (rx, ro) = remainXO lToSolve

countRemainDot :: String -> Int
countRemainDot l = rx + ro
  where
    (rx, ro) = remainXO l

--------------------------------

-- does even more
-- ? How does it work
-- check possible fills
-- filter combs to get only invalid positions
-- fill invalid positions with opposite chars
advancedTech1 :: String -> String
advancedTech1 l = if smallestLeft == 2 then spreadOnDots toSpread l else l
  where
    smallestLeft = let (rx, ro) = remainXO l in min rx ro
    (valids, invalids) = fillVariants l
    toSpread = map mapHelp result
    mapHelp cs = if length cs == 1 then opposite (head cs) else '.'
    opposite c
      | c == 'x' = 'o'
      | c == 'o' = 'x'
      | otherwise = '.'
    result = zipWith oneHasOtherNot (listCommons invalids) (listCommons valids)
