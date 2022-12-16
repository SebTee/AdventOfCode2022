module Day13.Part2 (solve) where

import Day13.Part1 (formatInp, parse, Tm)
import Data.List.Split
import Data.List (sort)

solve :: String -> String
solve inp = show $ product $ map fst $ filter (\(_, x) -> x `elem` dividers) $ zip [(1 :: Int) ..] sorted
  where sorted = sort $ (++) dividers $ concat $ formatInp inp

dividers :: [Tm]
dividers = map parse $ splitOn " " "[[2]] [[6]]"