module Day09.Part1 (solve, formatInp, tailJourney, headJourney) where

import Data.List.Split
import Data.List (nub)

type Pos = (Int, Int)

solve :: String -> String
solve = show . length . nub . tailJourney . headJourney . formatInp

formatInp :: String -> [Pos]
formatInp = concatMap getMove . splitOn "\n"

getMove :: String -> [Pos]
getMove ('L':_:n) = replicate (read n) (-1, 0)
getMove ('R':_:n) = replicate (read n) (1, 0)
getMove ('U':_:n) = replicate (read n) (0, 1)
getMove ('D':_:n) = replicate (read n) (0, -1)
getMove _ = error "invalid input"

headJourney :: [Pos] -> [Pos]
headJourney h = (0, 0) : hj (0, 0) h
  where 
    hj (x, y) ((i, j):ms) = newPos : hj newPos ms
      where newPos = (x + i, y + j)
    hj _ _ = []

tailJourney :: [Pos] -> [Pos]
tailJourney (h:hs) = h : tj h hs
  where
    tj t (ch:chs) = nt : tj nt chs
      where nt = moveTail t ch
    tj _ _ = []
tailJourney _ = [(0, 0)]
    
moveTail :: Pos -> Pos -> Pos
moveTail t h = case np of
  [] -> t
  [(_, p)] -> p
  _ -> error "invalid input"
  where
    ft = tailFromTo h
    np = filter (\(fs, _) -> t `elem` fs) ft
    
tailFromTo :: Pos -> [([Pos], Pos)]
tailFromTo (hx, hy) = map (\(px, py) -> (mvHere (px, py), (px + hx, py + hy))) cardDir
  where
    cardDir = [(x, y) | x <- [-1..1], y <- [-1..1], not $ (x == 0) && (y == 0)]
    mvHere (x, 0) = [((2 * x) + hx, hy + my) | my <- [-1..1]]
    mvHere (0, y) = [(hx + mx, (2 * y) + hy) | mx <- [-1..1]]
    mvHere (x, y) = [((x * 2) + hx, (y * 2) + hy)]
