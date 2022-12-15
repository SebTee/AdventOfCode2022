module Day12.Part1 (solve, formatInp, find, finds, findDist) where

import Data.List.Split
import Data.List (nub)
import Data.Maybe
import qualified Data.Map as Map
import Helper (indexes, replaceAt2d)

type Point = (Int, Int)
type Matrix = [String]

solve :: String -> String
solve inp = show $ findDist start end newM
  where
    m = formatInp inp
    start = find 'S' m
    end = find 'E' m
    newM = replaceAt2d start 'a' $ replaceAt2d end 'z' m
    
findDist :: Point -> Point -> Matrix -> Int
findDist start end m = fromMaybe maxBound (Map.lookup end $ findDists start m)
    
findDists :: Point -> Matrix -> Map.Map Point Int
findDists start m = fd 1 (adjIndexes m start) (Map.singleton start 0) 
  where
    fd _ [] checked = checked
    fd depth toCheck checked = fd (depth + 1) newToCheck newChecked
      where
        newToCheck = nub $ filter (`Map.notMember` newChecked) $ concatMap (adjIndexes m) toCheck
        newChecked = checks depth toCheck checked

checks :: Int -> [Point] -> Map.Map Point Int -> Map.Map Point Int
checks depth (p:ps) checked = checks depth ps $ Map.insert p depth checked
checks _ _ checked = checked

formatInp :: String -> Matrix
formatInp = splitOn "\n"

find :: Char -> Matrix -> Point
find target m = head $ finds target m

finds :: Char -> Matrix -> [Point]
finds target m = f [] $ indexes m
  where
    f v (i:is)
      | m `index` i == target = f (i : v) is
      | otherwise = f v is
    f v _ = v

adjIndexes :: Matrix -> Point -> [Point]
adjIndexes m (px, py) = filter f a
  where
    maxX = length m
    maxY = length $ head m
    h = m `index` (px, py)
    a = [(px + x, py + y) | x <- [-1 .. 1], y <- [-1 .. 1], (x == 0) /= (y == 0)]
    f (x, y) = and [0 <= x, x < maxX, 0 <= y, y < maxY, canStep h $ m `index` (x, y)]
    
canStep :: Enum a => a -> a -> Bool
canStep from to = fromEnum to <= (1 + fromEnum from)
    
index :: Matrix -> Point -> Char
index m (x, y) = (m !! x) !! y