module Day06.Part2 (solve) where
  
import Day06.Part1 (firstUniqueSubString)

solve :: String -> String
solve = show . firstUniqueSubString 14