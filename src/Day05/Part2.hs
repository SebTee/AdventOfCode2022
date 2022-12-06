module Day05.Part2 (solve) where
  
import Day05.Part1 (formatInp, setCraneRun, Move, Crates)
import Helper (replaceAt)

solve :: String -> String
solve = show . map head . uncurry runCraneOrd . formatInp

runCraneOrd :: Crates -> [Move] -> Crates
runCraneOrd = setCraneRun moveOrd

moveOrd :: Move -> Crates -> Crates
moveOrd (a, f, t) cs = replaceAt t newTo $ replaceAt f newFrom cs
  where
    from = cs !! f
    to = cs !! t
    newFrom = drop a from
    newTo = mc ++ to
    mc = take a from