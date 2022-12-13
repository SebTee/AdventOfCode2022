module Helper (modifyAt, replaceAt, isDivisible) where

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f l = case splitAt i l of
  (b, x:xs) -> b ++ f x : xs
  _ -> error "index out of bounds"
  
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x = modifyAt i (const x)

isDivisible :: Integral a => a -> a -> Bool
isDivisible x y = (x `mod` y) == 0