module Day11.Part1 (solve, formatInp, monkeyBusiness, processRound) where

import Data.List.Split
import Helper (isDivisible, replaceAt, modifyAt)
import Data.List (sort)

data Monkey = M [Int] (Int -> Int) (Int -> (Int, Int)) Int

instance Show Monkey where
  show (M x _ _ c) = "M " ++ show x ++ ' ' : show c
  
instance Eq Monkey where
  (M _ _ _ c1) == (M _ _ _ c2) = c1 == c2
  
instance Ord Monkey where
  compare (M _ _ _ c1) (M _ _ _ c2) = compare c1 c2

solve :: String -> String
solve = show . monkeyBusiness . (!! 20) . iterate (processRound (`div` 3)) . formatInp

processRound :: (Int -> Int) -> [Monkey] -> [Monkey]
processRound pf = pr 0
  where
    pr i ms
      | i < length ms = pr (i + 1) caught
      | otherwise = ms
      where
        thrownMonkey = processMonkey pf (ms !! i)
        thrown = replaceAt i (fst thrownMonkey) ms
        caught = catches thrown $ snd thrownMonkey
        
catch :: Int -> Monkey -> Monkey
catch item (M is p t c) = M (is ++ [item]) p t c

catches :: [Monkey] -> [(Int, Int)] -> [Monkey]
catches ms ((i, x):is) = catches (modifyAt i (catch x) ms) is
catches ms _ = ms

processMonkey :: (Int -> Int) -> Monkey -> (Monkey, [(Int, Int)])
processMonkey pf (M is p t c) = (M [] p t $ c + length is, map (t . pf . p) is)

formatInp :: String -> [Monkey]
formatInp = map formatMonkey . splitOn "\n\n"

formatMonkey :: String -> Monkey
formatMonkey m = M staringItems op test 0
  where
    mLines = splitOn "\n" m
    staringItems = map read $ splitOn ", " $ drop 18 (mLines !! 1)
    op = formatOperation $ drop 19 (mLines !! 2)
    test = formatTest $ drop 3 mLines

formatOperation :: String -> Int -> Int
formatOperation op = fOp parts
  where
    parts = splitOn " " op
    fOp ["old", strOp, "old"] x = strToOp strOp x x
    fOp ["old", strOp, constant] x = strToOp strOp x (read constant)
    fOp _ _ = error "invalid input"
    strToOp "+" = (+)
    strToOp "*" = (*)
    strToOp _ = error "invalid input"

formatTest :: [String] -> Int -> (Int, Int)
formatTest t x = if x `isDivisible` divisor then (true, x) else (false, x)
  where
    divisor = read $ drop 21 $ head t
    true = read $ drop 29 (t !! 1)
    false = read $ drop 30 (t !! 2)
    
monkeyBusiness :: [Monkey] -> Int
monkeyBusiness = product . take 2 . reverse . sort . map (\(M _ _ _ c) -> c)