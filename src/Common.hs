module Common
  ( loadInput,
    getSolution
  ) where

import Day01.Part1
import Day01.Part2
  
loadInput :: Int -> Int -> IO String
loadInput d p = readFile filePath
  where
    filePath = "./input/day" ++ day ++ "-" ++ part ++ ".txt"
    part = show p
    day
      | d < 10    = '0' : show d
      | otherwise = show d

getSolution :: Int -> Int -> (String -> String)
getSolution 1 1 = Day01.Part1.solve
getSolution 1 2 = Day01.Part2.solve
getSolution _ _ = error "Invalid day part"