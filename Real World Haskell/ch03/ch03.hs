-- exercise 1
length' :: Num a => [a1] -> a
length' [] = 0
length' (x:xs) = 1 + length' xs