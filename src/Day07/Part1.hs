module Day07.Part1 (solve, formatInp, getFsWhere, Fs(Dir, File), size) where

import Data.List (sort)
import Data.Bifunctor
import Data.List.Split
import Data.List.GroupBy
import Data.Either (fromRight, isLeft)

data Fs = Dir String [Fs] | File String Int deriving (Show)

data Cmd = Ls [Fs] | CdUp | Cd String deriving (Show)

solve :: String -> String
solve = show . sum . map size . getFsWhere (isDirSmallerThan 100000) . formatInp

formatInp :: String -> Fs
formatInp = toFs . getFps . splitCommands . map formatLine . tail . splitOn "\n"

formatLine :: String -> Either Cmd Fs
formatLine "$ ls" = Left $ Ls []
formatLine "$ cd .." = Left CdUp
formatLine ('$':cmd) = Left $ Cd $ splitOn " " cmd !! 2
formatLine ('d':dir) = Right $ Dir (splitOn " " dir !! 1) []
formatLine file = case splitOn " " file of
  [sz, name] -> Right $ File name $ read sz
  _ -> error "invalidInput"

splitCommands :: [Either Cmd Fs] -> [Cmd]
splitCommands (Left (Ls _):cs) = Ls fs : splitCommands subsequent
  where
    grouped = groupBy (\x -> (==) (isLeft x) . isLeft) cs
    fs = map (fromRight (File "" 0)) $ head grouped
    subsequent = concat $ tail grouped
splitCommands (Left c:cs) = c : splitCommands cs
splitCommands (Right _:_) = error "invalid input"
splitCommands [] = []

getFps :: [Cmd] -> [([String], Int)]
getFps = sort . map (Data.Bifunctor.first reverse) . gf []
  where
    gf :: [String] -> [Cmd] -> [([String], Int)]
    gf pwd (Cd n:cs) = gf (n : pwd) cs
    gf (_:pwd) (CdUp:cs) = gf pwd cs
    gf pwd (Ls fs:cs) = toPaths fs ++ gf pwd cs
      where
        toPaths :: [Fs] ->  [([String], Int)]
        toPaths (File n s:files) = (n : pwd, s) : toPaths files
        toPaths (Dir _ _:files) = toPaths files
        toPaths _ = []
    gf _ _ = []

toFs :: [([String], Int)] -> Fs
toFs = tf ""
  where
    tf pwd fps = Dir pwd $ map pop groups
      where
        g (x:_, _) (y:_, _) = x == y
        g _ _ = False
        groups = groupBy g fps
        strip (_:p, s) = (p, s)
        strip _ = error "invalid input"
        pop [([n], s)] = File n s
        pop sfps = tf newPwd $ map strip sfps
          where
            newPwd = head $ fst $ head sfps

getFsWhere :: (Fs -> Bool) -> Fs -> [Fs]
getFsWhere f (File n s) = [File n s | f (File n s)]
getFsWhere f (Dir n fs)
  | f (Dir n fs) = Dir n fs : rest
  | otherwise = rest
  where rest = concatMap (getFsWhere f) fs

size :: Fs -> Int
size (File _ s) = s
size (Dir _ fs) = sum $ map size fs

isDirSmallerThan :: Int -> Fs -> Bool
isDirSmallerThan x (Dir n fs) = size (Dir n fs) <= x
isDirSmallerThan _ _ = False