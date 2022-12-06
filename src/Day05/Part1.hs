module Day05.Part1 (solve, formatInp, setCraneRun, Move, Crates) where

import Data.List (transpose)
import Helper (replaceAt)
import Data.List.Split

type Move = (Int, Int, Int)
type Crates = [String]

solve :: String -> String
solve = map head . uncurry runCraneRev . formatInp

formatInp :: String -> (Crates, [Move])
formatInp i = case splitOn "\n\n" i of
  [x, y] -> (formatCrates x, map formatMove $ splitOn "\n" y)
  _ -> error "invalid input"

formatCrates :: String -> Crates
formatCrates = filter (not . null) . map (filter (not . (`elem` "[ ]"))) . transpose . init . splitOn "\n"

formatMove :: String -> Move
formatMove m = case map read $ words $ filter (`elem` ' ':['0'..'9']) m of
  [x, y, z] -> (x, y - 1, z - 1)
  _ -> error "invalid input"

runCraneRev :: Crates -> [Move] -> Crates
runCraneRev = setCraneRun moveRev
  
setCraneRun :: (Move -> Crates -> Crates) -> Crates -> [Move] -> Crates
setCraneRun f c (m:ms) = setCraneRun f (f m c) ms
setCraneRun _ c _ = c

moveRev :: Move -> Crates -> Crates
moveRev (a, f, t) cs = replaceAt t newTo $ replaceAt f newFrom cs
  where
    from = cs !! f
    to = cs !! t
    newFrom = drop a from
    newTo = reverse mc ++ to
    mc = take a from