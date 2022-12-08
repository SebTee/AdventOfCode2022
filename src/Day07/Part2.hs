module Day07.Part2 (solve) where

import Day07.Part1 (formatInp, getFsWhere, Fs(Dir), size)

solve :: String -> String
solve inp = show $ minimum $ filter (>= spaceNeeded) $ map size $ getFsWhere isDir fs
  where
    fs = formatInp inp
    freeSpace = 70000000 - size fs
    spaceNeeded = 30000000 - freeSpace

isDir :: Fs -> Bool
isDir (Dir _ _) = True
isDir _ = False