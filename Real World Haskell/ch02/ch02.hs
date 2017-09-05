--Real World Haskell
--ch.02 functions
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n-1) (tail xs)

isOdd n = mod n 2 == 1

myLastButOne :: [a] -> a
myLastButOne [] = error "No elements in list"
--mylastButOne [_] = _
myLastButOne (x : _ : []) = x
myLastButOne (_ : xs) = myLastButOne xs