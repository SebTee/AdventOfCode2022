module Day12.Part2 (solve) where

import Day12.Part1 (formatInp, find, finds, findDist)
import Helper (replaceAt2d)

solve :: String -> String
solve inp = show $ minimum $ map (\x -> findDist x end newM) ais
  where
    ais = finds 'a' newM
    m = formatInp inp
    start = find 'S' m
    end = find 'E' m
    newM = replaceAt2d start 'a' $ replaceAt2d end 'z' m