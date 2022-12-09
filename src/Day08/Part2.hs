module Day08.Part2 (solve) where

import Day08.Part1 (formatInp, getCardinalSlices)

solve :: String -> String
solve = show . maximum . scenicScores . formatInp

scenicScores :: [[Int]] -> [Int]
scenicScores trees = ss is
  where
    ss ((x, y):indexes) = scenicScore (trees !! x !! y) (getViewSlices (x, y) trees) : ss indexes
    ss _ = []
    is = [(x, y) | x <- [0 .. length trees - 1], y <- [0 .. length (head trees) - 1]]

scenicScore :: Int -> [[Int]] -> Int
scenicScore h views = product $ map ((+) 1 . findFirstValueIndex (>= h)) views

getViewSlices :: (Int, Int) -> [[a]] -> [[a]]
getViewSlices (x, y) ls = case getCardinalSlices (x, y) ls of
  [r1, a1, r2, a2] -> [reverse r1, a1, reverse r2, a2]
  _ -> error "invalid input"

findFirstValueIndex :: (a -> Bool) -> [a] -> Int
findFirstValueIndex = ffv 0
  where
    ffv i f (t:ts)
      | f t = i
      | otherwise = ffv (i + 1) f ts
    ffv i _ _ = i - 1