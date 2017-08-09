import Data.Char

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

threeEqual :: Eq a => a -> a -> a -> Bool
threeEqual m n p = (m == n) && (m == p) && (n == p)

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n o p = (threeEqual m n o) && (threeEqual m n p) && (threeEqual m o p) && (threeEqual n o p)

min' :: Ord t => t -> t -> t
min' x y = if x <= y then x else y

minThree :: Ord a => a -> a -> a -> a
minThree x y z
 | (x <= y) && (x <= z) = x
 | (y <= x) && (y <= z) = y
 | (z <= y) && (z <= x) = z

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

toUpper' :: Char -> Char
toUpper' ch = toEnum (fromEnum ch + offset)

charToNum :: Char -> Int
charToNum ch = if (isDigit ch == True) then digitToInt ch else 0

onThreeLines :: [Char] -> [Char] -> [Char] -> IO ()
onThreeLines x y z = putStr (x ++ "\n" ++ y ++ "\n" ++ z ++ "\n")