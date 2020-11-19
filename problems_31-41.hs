-- Problem 31

isPrime :: Int -> Bool
isPrime i = isPrimeHelper i [2..(div i 2)]

isPrimeHelper :: Int -> [Int] -> Bool
isPrimeHelper _ [] = True
isPrimeHelper i (m:ms)
  | (mod i m) == 0 = False
  | otherwise      = isPrimeHelper i ms

-- Problem 32

myGcd :: Int -> Int -> Int
myGcd a b
  | a == b = a
  | a > b  = myGcd (a - b) b
  | a < b  = myGcd a (b - a)

-- Problem 33

coprime x y = (myGcd x y) == 1

-- Problem 34

totient x = totientHelper 1 x

totientHelper :: Int -> Int -> Int
totientHelper i x
  | i == x      = 0
  | coprime i x = 1 + (totientHelper (i + 1) x)
  | otherwise   = totientHelper (i + 1) x

-- Problem 35

primeFactors y = pfHelper [2..y] y

pfHelper :: [Int] -> Int -> [Int]
pfHelper [] _ = []
pfHelper (x:xs) y
  | (isPrime x) && (isFactor x y) = x : pfHelper xs y
  | otherwise                     = pfHelper xs y

isFactor x y = (mod y x) == 0

-- Problem 36

primeFactorsMult y = pfmHelper (primeFactors y)

pfmHelper :: [Int] -> [(Int, Int)]
pfmHelper [x]    = [(x, 1)]
pfmHelper (x:xs) = if x == fst theHead then (x, ((snd theHead) + 1)):theRest
                                       else (x, 1):theResult
    where
        theResult = pfmHelper xs
        theHead   = head theResult
        theRest   = tail theResult

-- Problem 37

phi x = phi' (primeFactorsMult x)
    where
        phi' []     = 1
        phi' (x:xs) = (fst x - 1) * (fst x) ^ (snd x - 1) * phi' xs

-- Problem 39

primesR x y = primesR' [x..y]
    where
        primesR' []     = []
        primesR' (x:xs)
          | isPrime x = x : primesR' xs
          | otherwise = primesR' xs

-- Problem 40

goldbach x = goldbach' x (primesR 3 x)
    where
        goldbach' x (p:ps)
          | isPrime (x - p) = (p, (x - p))
          | otherwise       = goldbach' x ps

-- Problem 41

goldbachList x y = map goldbach $ filter isEven [x..y]

isEven x = if mod x 2 == 0 then True
                           else False

goldbachList' x y z = filter (primesAreBig z) $ goldbachList x y

primesAreBig z x = if fst x > z && snd x > z then True
                                             else False
