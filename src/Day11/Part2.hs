module Day11.Part2 (solve) where

import Data.List.Split
import Day11.Part1 (formatInp, monkeyBusiness, processRound)

solve :: String -> String
solve inp = show $ monkeyBusiness $ (!! 10000) $ iterate (processRound (`mod` lowestCommonMultiple)) $ formatInp inp
  where 
    factors = getFactors inp
    lowestCommonMultiple :: Int
    lowestCommonMultiple = foldl lcm 1 factors

getFactors :: String -> [Int]
getFactors = map (divisor . splitOn "\n") . splitOn "\n\n"
  where 
    divisor l = read $ drop 21 (l !! 3)
    