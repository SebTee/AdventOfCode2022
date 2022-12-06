module Day04.Part1 (solve, formatInp) where

import Data.List.Split

solve :: String -> String
solve = show . length . filter (uncurry eitherContains) . formatInp

eitherContains :: (Int, Int) -> (Int, Int) -> Bool
eitherContains x y = contains x y || contains y x

contains :: (Int, Int) -> (Int, Int) -> Bool
contains (outer1, outer2) (inner1, inner2) = outer1 <= inner1 && inner2 <= outer2

formatInp :: String -> [((Int, Int), (Int, Int))]
formatInp = map formatPair . splitOn "\n"
  where
    formatPair = pairToTuple . map formatRange . splitOn ","
    formatRange = pairToTuple . map read . splitOn "-"

pairToTuple :: [a] -> (a, a)
pairToTuple [x, y] = (x, y)
pairToTuple _ = error "invalid input"