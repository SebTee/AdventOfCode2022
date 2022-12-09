module Day09.Part2 (solve) where

import Day09.Part1 (formatInp, tailJourney, headJourney)
import Data.List (nub)

solve :: String -> String
solve = show . length . nub . (!! 9) . iterate tailJourney . headJourney . formatInp