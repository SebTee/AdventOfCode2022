module Day04.Part2 (solve) where
  
import Day04.Part1 (formatInp)

solve :: String -> String
solve = show . length . filter (uncurry overlap) . formatInp

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (x1, x2) (y1, y2) = (x2 >= y1 && y2 >= x1) || (y2 >= x1 && x2 >= y1)