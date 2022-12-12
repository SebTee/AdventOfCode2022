module Day10.Part2 (solve) where

import Day10.Part1 (formatInp, xAtCycle)
import Data.List.Split (chunksOf)

solve :: String -> String
solve inp = formatOut width $ screen ops width size
  where 
    ops = formatInp inp
    width = 40 :: Int
    height = 6 :: Int
    size = width * height

formatOut :: Int -> [Bool] -> String
formatOut width = unlines . chunksOf width . map (\x -> if x then '#' else '.')

screen :: [Int] -> Int -> Int -> [Bool]
screen ops width size = map (isPixelOn ops width) [0 .. size - 1]

isPixelOn :: [Int] -> Int -> Int -> Bool
isPixelOn ops width n = (n `mod` width) `elem` s
  where
    s = sprite $ xAtCycle ops (n + 1)

sprite :: Int -> [Int]
sprite n = [n + x | x <- [-1 .. 1]]