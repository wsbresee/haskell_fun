-- Problem 11

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode [a] = [(1, a)]
encode (x:xs)
    | x == headValue = (headInt + 1, x) : tail theEncode
    | otherwise      = (1, x):theEncode
    where theEncode = encode xs
          headValue = snd (head theEncode)
          headInt   = fst (head theEncode)

encode2 :: (Eq a) => [a] -> [ListItem a]
encode2 xs = map (encodeHelper) (encode xs)

encodeHelper :: (Int, a) -> ListItem a
encodeHelper (i, x)
    | i == 1    = (Single x)
    | otherwise = (Multiple i x)

-- Problem 12

decode :: [ListItem a] -> [a]
decode []     = []
decode (x:xs) = (decodeHelper x) ++ (decode xs)

decodeHelper :: ListItem a -> [a]
decodeHelper (Single x) = [x]
decodeHelper (Multiple i x) = makeListOf i x

makeListOf :: Int -> a -> [a]
makeListOf i x
    | i == 0    = []
    | otherwise = x : (makeListOf (i - 1) x)

-- Problem 13

-- Solution to problem 11 solves this one.

-- Problem 14

duplicate1 :: [a] -> [a]
duplicate1 (xs) = replicate1 xs 2

-- Problem 15

replicate1 :: [a] -> Int -> [a]
replicate1 [] _     = []
replicate1 (x:xs) i = (replicateHelper x i) ++ replicate1 xs i

replicateHelper :: a -> Int -> [a]
replicateHelper x i
    | i == 1    = [x]
    | otherwise = x : (replicateHelper x (i - 1))

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs j = dropEveryHelper xs 1 j

dropEveryHelper :: [a] -> Int -> Int -> [a]
dropEveryHelper [] _ _ = []
dropEveryHelper (x:xs) i j
    | i == j    = dropEveryHelper xs 1 j
    | otherwise = x : dropEveryHelper xs (i + 1) j

-- Problem 17

split :: [a] -> Int -> [[a]]
split [] _ = [[]]
split (x:xs) i
    | i == 1    = [x] : theResult
    | i > 1     = (x : theHead) : theTail
    | otherwise = [x : theHead]
    where
        theResult = split xs (i - 1)
        theHead = head theResult
        theTail = tail theResult

split2 :: [a] -> Int -> ([a],[a])
split2 [] _ = ([],[])
split2 (x:xs) i
    | i >= 1     = ((x : fst result) , (snd result))
    | otherwise  = ((fst result) , (x : snd result))
    where
        result = split2 xs (i - 1)

-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice xs i j = trimBeginning (trimEnd xs j) i

trimBeginning :: [a] -> Int -> [a]
trimBeginning (x:xs) i
  | i > 2    = trimBeginning xs (i-1)
  | otherwise = xs

trimEnd :: [a] -> Int -> [a]
trimEnd (x:xs) i
  | i > 0    = x : (trimEnd xs (i-1))
  | otherwise = []

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate xs i = (snd theSplits) ++ (fst theSplits)
  where
      placeToSplit = if (i >= 0) then i else (i + (length xs))
      theSplits = split2 xs placeToSplit

-- Problem 20

removeAt :: Int -> [a] -> (a, [a])
removeAt i [x]
  | i == 1      = (x, [])
  | otherwise   = (x, [x])
removeAt i (x:xs)
  | i == 1      = (x, theSecond)
  | otherwise   = (theFirst, x:theSecond)
  where
      removeAt' = removeAt (i - 1) xs
      theFirst  = fst removeAt'
      theSecond = snd removeAt'
