module Day08.Part1 (solve, formatInp, getCardinalSlices) where

import Data.List.Split
import Data.List (transpose)

solve :: String -> String
solve = show . countVisible . formatInp

formatInp :: String -> [[Int]]
formatInp = map (map (\x -> read [x]) ) . splitOn "\n"

countVisible :: [[Int]] -> Int
countVisible trees = length $ filter (== True) $ countVisTrees is
  where
    is = [(x, y) | x <- [0 .. length trees - 1], y <- [0 .. length (head trees) - 1]]
    visible :: Int -> [[Int]] -> Bool
    visible h adj = any (all (< h)) adj
    countVisTrees ((x, y):indexes) = visible (trees !! x !! y) (getCardinalSlices (x, y) trees) : countVisTrees indexes
    countVisTrees _ = []

getCardinalSlices :: (Int, Int) -> [[a]] -> [[a]]
getCardinalSlices (x, y) ls = slice (x, y) ls ++ slice (y, x) (transpose ls)

slice :: (Int, Int) -> [[a]] -> [[a]]
slice (x, y) ls = case splitAt y $ ls !! x of
  (a, b) -> [a, tail b]