square a = a * a

countChange :: (Integral a) => a -> a
countChange amount = cc amount 5

cc :: (Integral a) => a -> a -> a
cc amount kinds 
  | amount == 0              = 1
  | amount < 0 || kinds == 0 = 0
  | otherwise                = cc amount (kinds - 1) + cc (amount - fd kinds) kinds

fd :: (Integral a) => a -> a
fd 1 = 1
fd 2 = 5
fd 3 = 10
fd 4 = 25
fd 5 = 50
fd n = 50

smallestDivisor :: (Integral a) => a -> a
smallestDivisor n = findDiv n 2

findDiv :: (Integral a) => a -> a -> a
findDiv n testDiv
  | (square testDiv) > n = n
  | divides testDiv n    = testDiv
  | otherwise            = findDiv n (testDiv + 1)
  where
    divides a b = (rem b a) == 0

piSum :: (Ord a, Fractional a) => a -> a -> a
piSum a b
  | a > b     = 0
  | otherwise = (1 / ((a + 2) * a)) + piSum (a + 4) b

average a b = (a + b) / 2

search :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a
search f n p
  | closeEnough n p = midpoint
  | testValue > 0   = search f n midpoint
  | testValue < 0   = search f midpoint p
  | otherwise       = midpoint
  where
   testValue       = f midpoint
   midpoint        = average n p
   closeEnough x y = abs (x - y) < 0.001

halfIntervalMethod :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a
halfIntervalMethod f a b
  | av < 0 && bv > 0 = search f a b
  | bv < 0 && av > 0 = search f b a
  | otherwise        = error "Values are not of opposite sign"
  where
    av = f a
    bv = f b

tolerance :: (Fractional a) => a
tolerance = 0.00001

fixedPoint :: (Ord a, Fractional a) => (a -> a) -> a -> a
fixedPoint f firstGuess = try firstGuess
  where
    closeEnough v1 v2 = abs (v1 - v2) < tolerance
    try guess
      | closeEnough guess next = next
      | otherwise              = try next
      where
        next = f guess
 
sqrt' x = fixedPoint (\y -> average y (x / y)) 1.0
