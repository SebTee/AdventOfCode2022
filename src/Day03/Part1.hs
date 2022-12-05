module Day03.Part1 (solve, letterScore, commonElements) where

import Data.List.Split
import Data.Char (ord)

solve :: String -> String
solve = show . sum . map (letterScore . head . uncurry comp) . formatInp

formatInp :: String -> [(String, String)]
formatInp = map splitMiddle . splitOn "\n"
  where 
    splitMiddle l = splitAt (length l `div` 2) l
    
letterScore :: Char -> Int
letterScore x = case ord x of
  y 
    | y < 91 -> y - 38
    | otherwise -> y - 96
    
commonElements :: (Eq a) => [[a]] -> [a]
commonElements [xs, ys] = comp ys xs
commonElements (xs:ys:lss) = commonElements $ comp xs ys : lss
commonElements [x] = x
commonElements _ = []

comp :: (Eq a) => [a] -> [a] -> [a]
comp a = filter (`elem` a)