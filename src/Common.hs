module Common
  ( loadInput,
    getSolution
  ) where

import Day01.Part1 (solve)
import Day01.Part2 (solve)
import Day02.Part1 (solve)
import Day02.Part2 (solve)
  
loadInput :: Int -> IO String
loadInput d = readFile filePath
  where
    filePath = "./input/day" ++ day ++ ".txt"
    day
      | d < 10    = '0' : show d
      | otherwise = show d

getSolution :: Int -> Int -> (String -> String)
getSolution 1 1 = Day01.Part1.solve
getSolution 1 2 = Day01.Part2.solve
getSolution 2 1 = Day02.Part1.solve
getSolution 2 2 = Day02.Part2.solve
getSolution _ _ = error "Invalid day part"