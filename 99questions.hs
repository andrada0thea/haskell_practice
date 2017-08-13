-- exercises from https://wiki.haskell.org/99_questions/

-- Problem 1
myLast :: [a] -> a
myLast [] = error "No end for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast = last . init

-- Problem 3
elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "Index out of bounds"
elementAt' (x:_) 1 = x
elementAt' (_:xs) n
 | n < 1 = error "Index out of bounds"
 | otherwise = elementAt' xs (n-1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
