-- random stuff I've done
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
