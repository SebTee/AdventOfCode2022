module Day13.Part1 (solve, formatInp, parse, Tm) where

import Data.List.Split
import Data.List (sort)

data Tm = L [Tm] | I Int deriving(Eq, Show)

solve :: String -> String
solve = show . sum . map fst . filter snd . zip [(1 :: Int) ..] . map (\x -> x == sort x) . formatInp

formatInp :: String -> [[Tm]]
formatInp = map (map parse . splitOn "\n") . splitOn "\n\n"

instance Ord Tm where
  compare (L x) (L y) = compare x y
  compare (I x) (I y) = compare x y
  compare (L x) y = compare x [y]
  compare x (L y) = compare [x] y

parse :: String -> Tm
parse ('[':x) = L $ map parse splitTransmission
  where splitTransmission = splitForParse $ init x
parse x = I $ read x

splitForParse :: String -> [String]
splitForParse = sfp 0 []
  where
    sfp :: Int -> String -> String -> [String]
    sfp 0 s (',':cs) = s : sfp 0 [] cs
    sfp d s ('[':cs) = sfp (d + 1) (s ++ "[") cs
    sfp d s (']':cs) = sfp (d - 1) (s ++ "]") cs
    sfp d s (c:cs) = sfp d (s ++ [c]) cs
    sfp _ [] _ = []
    sfp _ s _ = [s]