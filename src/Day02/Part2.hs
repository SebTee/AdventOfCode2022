module Day02.Part2 (solve) where

import Data.List.Split
import Day02.Part1 (Game, Shape(Rock, Paper, Scissors), strategyScore, letterToRps)

solve :: String -> String
solve = show . sum . map strategyScore . formatInp

formatInp :: String -> [Game]
formatInp = map formatGame . splitOn "\n"
  where
    formatGame [x, _, y] = convert (letterToRps x) y
    formatGame _ = error "invalid input"
    
convert :: Shape -> Char -> Game
convert x 'Y' = (x, x)
convert Rock 'X' = (Rock, Scissors)
convert Rock 'Z' = (Rock, Paper)
convert Paper 'X' = (Paper, Rock)
convert Paper 'Z' = (Paper, Scissors)
convert Scissors 'X' = (Scissors, Paper)
convert Scissors 'Z' = (Scissors, Rock)
convert _ _ = error "invalid input"

