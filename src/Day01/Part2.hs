module Day01.Part2 (solve) where

import Day01.Part1 (formatInp)
import Data.List (sort)

solve :: String -> String
solve = show . sum . take 3 . reverse . sort . map sum . formatInp