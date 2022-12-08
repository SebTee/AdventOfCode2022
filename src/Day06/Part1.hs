module Day06.Part1 (solve, firstUniqueSubString) where

import Data.List (nub)

solve :: String -> String
solve = show . firstUniqueSubString 4
    
firstUniqueSubString :: (Eq a) => Int -> [a] -> Int
firstUniqueSubString offset = (offset +) . getIndex (firstUnique offset)

getIndex :: ([a] -> Bool) -> [a] -> Int
getIndex = getI 0
  where
    getI :: Int -> ([a] -> Bool) -> [a] -> Int
    getI i f l
      | f l = i
      | otherwise = getI (i + 1) f (tail l)
      
allUnique :: (Eq a) => [a] -> Bool
allUnique l = length l == length (nub l)

firstUnique :: (Eq a) => Int -> [a] -> Bool
firstUnique x = allUnique . take x