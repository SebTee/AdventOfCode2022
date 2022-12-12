module Day10.Part1 (solve, formatInp, xAtCycle) where

import Data.List.Split

solve :: String -> String
solve inp = show $ sum [signalStrength ops (20 + (40 * x)) | x <- [0 .. 5]]
  where ops = formatInp inp

formatInp :: String -> [Int]
formatInp = (:) 1 . concatMap formatOp . splitOn "\n"

formatOp :: String -> [Int]
formatOp "noop" = [0]
formatOp op = [0, read $ drop 5 op]

signalStrength :: [Int] -> Int -> Int
signalStrength ops n = n * xAtCycle ops n

xAtCycle :: [Int] -> Int -> Int
xAtCycle ops n = sum (take n ops)