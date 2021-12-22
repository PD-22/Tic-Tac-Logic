module Solvers where

import Helpers
  ( countRemainDot,
    doOnRowsCols,
    doWhileChanges,
    fillVariants,
    fillVariants2,
    mergeCombs,
    onlyValidSpread,
    remainXO,
    replace,
    replaceDot,
    spreadOnDots,
  )

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
      | null invalidCombs = emptySpread
      | otherwise = mergeCombs $ invertInvalid invalidCombs
    (validCombs, invalidCombs) = fillVariants l
    invertInvalid = map (replace charToGuess otherChar . replace otherChar '.')
    emptySpread = replicate (rx + ro) '.'
    (rx, ro) = remainXO l
    (charToGuess, otherChar)
      | rx == 1 && ro > 1 = ('x', 'o')
      | ro == 1 && rx > 1 = ('o', 'x')
      | otherwise = ('N', 'N')

completeLine :: String -> String
completeLine l
  | o == 0 && x > 0 = replaceDot 'x' l
  | x == 0 && o > 0 = replaceDot 'o' l
  | otherwise = l
  where
    (x, o) = remainXO l

avoidDuplication :: [String] -> [String]
avoidDuplication g = doWhileChanges (doOnRowsCols (map avoidDuplHelp3)) g
  where
    avoidDuplHelp3 l =
      let remainDots = countRemainDot l
          valids = fst (fillVariants2 l g)
       in if remainDots == 2 && (length valids == 1)
            then spreadOnDots (head valids) l
            else l

advancedTech1 :: String -> String
advancedTech1 l = if fewestLeft == 2 then spreadOnDots toSpread l else l
  where
    fewestLeft = let (rx, ro) = remainXO l in min rx ro
    toSpread = onlyValidSpread l
