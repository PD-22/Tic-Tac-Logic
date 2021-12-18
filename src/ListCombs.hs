module ListCombs where

import Data.List (sort)

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
