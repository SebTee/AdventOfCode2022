module Day03.Part2 (solve) where

import Data.List.Split
import Day03.Part1 (letterScore, commonElements)

solve :: String -> String
solve = show . sum . map (letterScore . head . commonElements) . formatInp

formatInp :: String -> [[String]]
formatInp = groupBy3 . splitOn "\n"
  where
    groupBy3 (x:y:z:ls) = [x, y, z] : groupBy3 ls
    groupBy3 [] = []
    groupBy3 _ = error "invalidInput"