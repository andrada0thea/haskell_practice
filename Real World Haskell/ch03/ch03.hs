import Data.List

-- exercise 1
length' :: Num a => [a1] -> a
length' [] = 0
length' (x:xs) = 1 + length' xs

-- exercise 3
meanListElements :: Foldable t => t Int -> Int
meanListElements x = sum x `div` (length x)

-- exercise 4
makePalindrome :: [a] -> [a]
makePalindrome x = x ++ reverse x

-- exercise 5
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome x
 | x == reverse x = True
 | x /= reverse x = False

-- exercise 6
sortSublists :: Ord a => [a] -> [a]
sortSublists xs = sortBy compare xs

-- exercise 7 & 8
intersperse' :: a -> [[a]] -> [a]
intersperse' separator [] = []
intersperse' separator (x:[]) = x
intersperse' separator (x:xs) = x ++ [separator] ++ intersperse' separator xs

-- exercise 9
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

calculateHeight :: (Num t, Ord t) => Tree t1 -> t
calculateHeight Empty = 0
calculateHeight (Node _ left right) = 1 + max (calculateHeight left) (calculateHeight right)