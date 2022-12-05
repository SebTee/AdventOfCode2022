module Day01.Part1 (solve, formatInp) where

import Data.List.Split

solve :: String -> String
solve = show . maximum . map sum . formatInp 

formatInp :: String -> [[Int]]
formatInp = map (map read . splitOn "\n") . splitOn "\n\n"
