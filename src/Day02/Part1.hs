module Day02.Part1 (solve, Game, Shape(Rock, Paper, Scissors), strategyScore, letterToRps) where

import Data.List.Split

type Game = (Shape, Shape)

data Shape = Rock | Paper | Scissors deriving (Show, Eq)

solve :: String -> String
solve = show . sum . map strategyScore . formatInp

letterToRps :: Char -> Shape
letterToRps 'A' = Rock
letterToRps 'X' = Rock
letterToRps 'B' = Paper
letterToRps 'Y' = Paper
letterToRps 'C' = Scissors
letterToRps 'Z' = Scissors
letterToRps _ = error "invalid input"

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3
  
gameScore :: Game -> Int
gameScore (Rock, Paper) = 6
gameScore (Paper, Scissors) = 6
gameScore (Scissors, Rock) = 6
gameScore (x, y)
  | x == y = 3
  | otherwise = 0
    
strategyScore :: Game -> Int
strategyScore (x, y) = gameScore (x, y) + shapeScore y

formatInp :: String -> [Game]
formatInp = map formatGame . splitOn "\n"
  where
    formatGame [x, _, y] = (letterToRps x, letterToRps y)
    formatGame _ = error "invalid input"
