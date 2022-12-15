module Helper (modifyAt, replaceAt, isDivisible, adj, indexes, modifyAt2d, replaceAt2d) where

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f l = case splitAt i l of
  (b, x:xs) -> b ++ f x : xs
  _ -> error "index out of bounds"
  
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x = modifyAt i (const x)

modifyAt2d :: (Int, Int) -> (a -> a) -> [[a]] -> [[a]]
modifyAt2d (x, y) f = modifyAt x (modifyAt y f)

replaceAt2d :: (Int, Int) -> a -> [[a]] -> [[a]]
replaceAt2d p c = modifyAt2d p (const c)

isDivisible :: Integral a => a -> a -> Bool
isDivisible x y = (x `mod` y) == 0

adj :: Enum a => a -> a -> Bool
adj x y = (fromEnum x - fromEnum y) `elem` [-1 .. 1]

indexes :: [[a]] -> [(Int, Int)]
indexes m = [(x, y) | x <- [0 .. length m - 1], y <- [0 .. length (head m) - 1]]