module Solvers where

import Helpers
  ( Cell (..),
    Grid,
    Line,
    countDot,
    doOnRowsCols,
    doWhileChanges,
    fillVariants,
    fillVariants2,
    mergeCombs,
    onlyValidSpread,
    onlyValidSpread2,
    remainXO,
    replace,
    replaceDot,
    spreadOnDots,
  )

avoidTriple1 :: Line -> Line
avoidTriple1 [] = []
avoidTriple1 [a] = [a]
avoidTriple1 [a, b] = [a, b]
avoidTriple1 (E : X : X : bs) = O : avoidTriple1 (X : X : bs)
avoidTriple1 (X : X : E : bs) = X : X : avoidTriple1 (O : bs)
avoidTriple1 (E : O : O : bs) = X : avoidTriple1 (O : O : bs)
avoidTriple1 (O : O : E : bs) = O : O : avoidTriple1 (X : bs)
avoidTriple1 (c : cs) = c : avoidTriple1 cs

avoidTriple2 :: Line -> Line
avoidTriple2 [] = []
avoidTriple2 [a] = [a]
avoidTriple2 [a, b] = [a, b]
avoidTriple2 (X : E : X : bs) = X : O : avoidTriple2 (X : bs)
avoidTriple2 (O : E : O : bs) = O : X : avoidTriple2 (O : bs)
avoidTriple2 (c : cs) = c : avoidTriple2 cs

-- if only one valid comb spread that
-- else spread merged inverted invalid combs
avoidTriple3 :: Line -> Line
avoidTriple3 [] = []
avoidTriple3 l
  | charToGuess == N = l
  | otherwise = spreadOnDots combToSpread l
  where
    combToSpread
      | length validCombs == 1 = head validCombs
      | null invalidCombs = emptySpread
      | otherwise = mergeCombs invertedInvalid
    (validCombs, invalidCombs) = fillVariants l
    invertedInvalid = map (replace charToGuess otherChar . replace otherChar E) invalidCombs
    emptySpread = replicate (rx + ro) E
    (rx, ro) = remainXO l
    (charToGuess, otherChar)
      | rx == 1 && ro > 1 = (X, O)
      | ro == 1 && rx > 1 = (O, X)
      | otherwise = (N, N)

completeLine :: Line -> Line
completeLine l
  | o == 0 && x > 0 = replaceDot X l
  | x == 0 && o > 0 = replaceDot O l
  | otherwise = l
  where
    (x, o) = remainXO l

avoidDuplication :: Grid -> Grid
avoidDuplication g = doWhileChanges (doOnRowsCols mapper) g
  where
    mapper g = map (avoidDupHelp g) g
    avoidDupHelp g l =
      let remainDots = countDot l
          (valids, _) = fillVariants2 l g
       in if remainDots == 2 && (length valids == 1)
            then spreadOnDots (head valids) l
            else l

advancedTech1 :: Line -> Line
advancedTech1 l = if fewestLeft == 2 then spreadOnDots toSpread l else l
  where
    fewestLeft = let (rx, ro) = remainXO l in min rx ro
    toSpread = onlyValidSpread l

advancedTech2 :: Grid -> Grid
advancedTech2 g = doWhileChanges (doOnRowsCols mapper) g
  where
    mapper g = map (advancedTech2Help g) g
    advancedTech2Help g l =
      let fewestLeft = let (rx, ro) = remainXO l in min rx ro
          toSpread = onlyValidSpread2 l g
       in if fewestLeft == 1 then spreadOnDots toSpread l else l
