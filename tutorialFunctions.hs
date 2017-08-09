-- tutorial functions
-- http://learnyouahaskell.com
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- factorial :: Integer -> Integer
-- factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you're out of luck"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not in between 1 and 3"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

buyPepsi :: (RealFloat a) => a -> String
buyPepsi change 
 | change < 1.20 = "You can't buy a Pepsi. Search for more change."
 | change == 1.20 = "You can buy a Pepsi!"
 | otherwise = "Go for a meal deal maybe?"

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
 | bmi <= skinny = "You're underweight, you emo, you!"  
 | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
 | bmi <= fat = "You're fat! Lose some weight, fatty!"  
 | otherwise = "You're a whale, congratulations!" 
 where bmi = weight / height ^ 2
       skinny = 18.5
       normal = 25.0
       fat = 30.0
-- where bmi = weight / height ^ 2
--       (skinny, normal, fat) = (18.5, 25.0, 30.0)

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 | a > b = GT
 | a == b = EQ
 | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
 where (f:_) = firstname
       (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
 where bmi weight height = weight / height ^ 2 
-- calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
 let sideArea = 2 * pi * r * h
     topArea = pi * r ^ 2
 in sideArea + 2 * topArea

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"
-- describeList xs = "The list is " ++ what xs
--  where what [] = "empty"
--        what [x] = "a singleton list"
--        what xs = "a longer list"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
 | x > maxTail = x
 | otherwise = maxTail
 where maxTail = maximum' xs
-- maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
 | n <= 0 = []
 | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
 | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse':: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
 | a == x = True
 | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
 let smallerSorted = quicksort [a | a <- xs, a <= x]
     biggerSorted = quicksort [a | a <- xs, a > x]
 in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- curried function
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred = compare 100

-- infix function
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' _ [] _ = []
zipWith'' _ _ [] = []
zipWith'' f (x:xs) (y:ys) = f x y : zipWith'' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
 where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
 | p x = x : filter p xs
 | otherwise = filter p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
 let smallerSorted = quicksort' (filter (<=x) xs)
     biggerSorted = quicksort' (filter (>x) xs)
 in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [10000, 9999..])
 where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
 | even n = n:chain (n `div` 2)
 | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
 where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

-- sum'' :: (Num a) => [a] -> a
-- sum'' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) $ map sqrt [1..])) + 1

