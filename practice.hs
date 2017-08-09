threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m /= n) && (m /= p) && (n /= p)

myLast :: [a] -> a
myLast [] = error "No end for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast = last . init

elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "Index out of bounds"
elementAt' (x:_) 1 = x
elementAt' (_:xs) n
 | n < 1 = error "Index out of bounds"
 | otherwise = elementAt' xs (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

addVecThreeFST :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
addVecThreeFST a b = (first a + first b, second a + second b, third a + third b)

addVecThreeSimplified :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
addVecThreeSimplified (a1, b1, c1) (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs