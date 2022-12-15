module Common
  ( loadInput,
    getSolution
  ) where

import Day01.Part1 (solve)
import Day01.Part2 (solve)
import Day02.Part1 (solve)
import Day02.Part2 (solve)
import Day03.Part1 (solve)
import Day03.Part2 (solve)
import Day04.Part1 (solve)
import Day04.Part2 (solve)
import Day05.Part1 (solve)
import Day05.Part2 (solve)
import Day06.Part1 (solve)
import Day06.Part2 (solve)
import Day07.Part1 (solve)
import Day07.Part2 (solve)
import Day08.Part1 (solve)
import Day08.Part2 (solve)
import Day09.Part1 (solve)
import Day09.Part2 (solve)
import Day10.Part1 (solve)
import Day10.Part2 (solve)
import Day11.Part1 (solve)
import Day11.Part2 (solve)
import Day12.Part1 (solve)
import Day12.Part2 (solve)
  
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
getSolution 3 1 = Day03.Part1.solve
getSolution 3 2 = Day03.Part2.solve
getSolution 4 1 = Day04.Part1.solve
getSolution 4 2 = Day04.Part2.solve
getSolution 5 1 = Day05.Part1.solve
getSolution 5 2 = Day05.Part2.solve
getSolution 6 1 = Day06.Part1.solve
getSolution 6 2 = Day06.Part2.solve
getSolution 7 1 = Day07.Part1.solve
getSolution 7 2 = Day07.Part2.solve
getSolution 8 1 = Day08.Part1.solve
getSolution 8 2 = Day08.Part2.solve
getSolution 9 1 = Day09.Part1.solve
getSolution 9 2 = Day09.Part2.solve
getSolution 10 1 = Day10.Part1.solve
getSolution 10 2 = Day10.Part2.solve
getSolution 11 1 = Day11.Part1.solve
getSolution 11 2 = Day11.Part2.solve
getSolution 12 1 = Day12.Part1.solve
getSolution 12 2 = Day12.Part2.solve
getSolution _ _ = error "Invalid day part"