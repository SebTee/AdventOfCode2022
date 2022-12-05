module Main (main) where

import System.Environment
import Common

main :: IO ()
main = do
  args <- getArgs
  case args of
    [d, p] -> do
      inp <- loadInput day
      putStrLn $ getSolution day part inp
      where
        day  = read d :: Int
        part = read p :: Int
    _ -> error "Invalid day part"