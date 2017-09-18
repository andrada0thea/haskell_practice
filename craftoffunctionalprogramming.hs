-- exercises from 
-- "Haskell: The Craft of Functional Programming (3rd Edition)" 
-- by Simon Thompson
import Data.Char

-- 3.9
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m /= n) && (m /= p) && (n /= p)

-- 3.10
threeEqual :: Eq a => a -> a -> a -> Bool
threeEqual m n p = (m == n) && (m == p) && (n == p)

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n o p = (threeEqual m n o) && (threeEqual m n p) && (threeEqual m o p) && (threeEqual n o p)

-- 3.14
min' :: Ord t => t -> t -> t
min' x y = if x <= y then x else y

minThree :: Ord a => a -> a -> a -> a
minThree x y z
 | (x <= y) && (x <= z) = x
 | (y <= x) && (y <= z) = y
 | (z <= y) && (z <= x) = z

-- 3.16
offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

toUpper' :: Char -> Char
toUpper' ch = toEnum (fromEnum ch + offset)

-- 3.17
charToNum :: Char -> Int
charToNum ch = if (isDigit ch == True) then digitToInt ch else 0

-- 3.18
onThreeLines :: [Char] -> [Char] -> [Char] -> IO ()
onThreeLines x y z = putStr (x ++ "\n" ++ y ++ "\n" ++ z ++ "\n")

-- 3.20
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z
 | (x > ceiling (averageThree x y z)) && (y > ceiling(averageThree x y z)) && (z > ceiling(averageThree x y z)) = 3
 | ((x > ceiling(averageThree x y z)) && (y > ceiling(averageThree x y z))) || ((x > ceiling(averageThree x y z)) && (z > ceiling(averageThree x y z))) || ((z > ceiling(averageThree x y z)) && (y > ceiling(averageThree x y z))) = 2
 | (x > ceiling(averageThree x y z)) || (y > ceiling(averageThree x y z)) || (z > ceiling(averageThree x y z)) = 1
 | otherwise = 0

-- 3.22
numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
 | b^2 > 4.0 * a * c = 2
 | b^2 == 4.0 * a * c = 1
 | b^2 < 4.0 * a * c = 0

-- 3.23
numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c
 | b /= 0.0 = 1
 | b == 0.0 && c /= 0.0 = 0
 | a /= 0.0 && b == 0.0 && c == 0.0 = 3

-- 3.24
smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = (-b - sqrt(b^2 - 4.0 * a * c))/ 2 * a
largerRoot a b c = (-b + sqrt(b^2 - 4.0 * a * c))/ 2 * a

-- 4.1
max' :: Integer -> Integer -> Integer
max' x y 
 | x >= y = x
 | otherwise = y

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max' z $ max' x y

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour x y z w 
 | x >= maxThree y z w = x
 | y >= maxThree x z w = y
 | z >= maxThree x y w = z
 | w >= maxThree x y z = w

maxFour' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour' x y z w = max' w $ maxThree x y z  

-- 4.2

-- 4.3
howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual m n p 
 | m == n && m == p = 3
 | (m == n && m /= p) || (n == p && n /= m) || (m == p && m /= n) = 2
 | m /= n && m /= p && n /= p = 0

-- 4.4 -- not finished
howManyOfFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer
howManyOfFourEqual x y z w
 | (howManyEqual x y z == 3) && (howManyEqual x y w == 3) = 4
 | (howManyEqual x y z == 3 || howManyEqual x y w == 3 || 
    howManyEqual x z w == 3 || howManyEqual y z w == 3) && 
   (howManyEqual x y z == 2 || howManyEqual x y w == 2 || 
    howManyEqual x z w == 2 || howManyEqual y z w == 2) = 3

-- 4.5

-- 4.6

-- 4.7

-- 4.8
triArea :: Float -> Float -> Float -> Float
triArea a b c
 | possible = sqrt(s*(s-a)*(s-b)*(s-c))
 | otherwise = 0
 where 
  s = (a+b+c)/2
  possible = a >= 0 && b >= 0 && c >= 0 && (a + b > c || a + c > b || b + c > a)

-- 4.9
maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs x y z
 | threeEqual x y z = (x,3)
 |(x == y && x == m) || (y == z && y == m) || (x == y && y == m) = (m, 2)
 |otherwise = (m, 1)
  where
   m = maxThree x y z

-- 4.3 chapter
data Move = Rock | Paper | Scissors deriving (Show, Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

-- 4.11
data Result = Win | Lose | Draw deriving (Show)

-- 4.12
outcome :: Move -> Move -> Result
outcome x y
 | x == y = Draw 
 | beat y == x = Win
 | otherwise = Lose

-- 4.15
data Temp = Cold | Hot deriving (Eq, Show, Ord)

data Season = Winter | Spring | Summer | Autumn deriving (Show, Eq)

tempFromSeason :: Season -> Temp
tempFromSeason season
 | season == Winter = Cold
 | season == Summer = Hot
 | season == Autumn = Cold
 | season == Spring = Cold

-- 4.16
data Month = December | January | February | March | April | May | June | July | August | September | October | November deriving (Show, Eq)

seasonFromMonth :: Month -> Season
seasonFromMonth x
 | x == December || x == January || x == February = Winter
 | x == March || x == April || x == May = Spring
 | x == June || x == July || x == August = Summer
 | x == September || x == October || x == November = Autumn 

-- 4.17
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
 | (n < m) = 0
 | (m == n) = m
 | (m + 1 == n) = m * n
 | otherwise = m * rangeProduct (m+1) (n-1) * n

-- 4.18
fac :: Integer -> Integer
fac x
 | (x < 0) = 0
 | (x == 0 || x == 1) = x
 | otherwise = rangeProduct 1 x

-- 4.19
natNumbersMult :: Num a => [a] -> a
natNumbersMult [] = 1
natNumbersMult (x:xs) = natNumbersMult xs * x 

-- 4.20
integerSqrt :: (RealFrac a, Integral b, Floating a) => a -> b
integerSqrt x = floor (sqrt x)

square :: Integer -> Integer
square n = n * n

mySqrt :: Integer -> Integer
mySqrt n = sqrtIter n 1

sqrtIter :: Integer -> Integer -> Integer
sqrtIter n guess
 |(square guess == n) = guess
 |(square next) > n = guess
 |otherwise = sqrtIter n next
  where
   next = guess + 1

-- 4.23
sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n
 | n == 0 = f 0
 | n > 0 = sumFun f (n-1) + f n

regions :: Integer -> Integer
regions n = (sumFun id n) + 1