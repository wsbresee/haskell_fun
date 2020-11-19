-- Problem 1

myLast1 x = last x

myLast2 :: [a] -> a
myLast2 [] = error "Empty list"
myLast2 [x] = x
myLast2 (_:xs) = myLast2 xs

-- Problem 2

myButLast1 :: [a] -> a
myButLast1 [x,_] = x
myButLast1 (x:xs) = myButLast1 xs

myButLast2 :: [a] -> a
myButLast2 (x:xs) =
    if length xs == 1 then x
    else myButLast2 xs

-- Problem 3

elementAt1 :: [a] -> Int -> a
elementAt1 [] _ = error "Empty list"
elementAt1 (x:xs) y  =
    if length xs == y + 2 then x
    else elementAt1 xs y

-- Problem 4

myLength1 x = length x

myLength2 :: [a] -> Int
myLength2 [] = 0
myLength2 [x] = 1
myLength2 (x:xs) = myLength2 xs + 1

-- Problem 5

myReverse1 x = reverse x

myReverse2 :: [a] -> [a]
myReverse2 [] = []
myReverse2 [x] = [x]
myReverse2 (x:xs) = (myReverse2 xs) ++ [x]

-- Problem 6

isPalindrome1 :: (Eq a) => [a] -> Bool
isPalindrome1 x
    | x == reverse x = True
    | otherwise      = False

-- Problem 7

data NestedList a = Elem a | List [NestedList a]

myFlatten1 :: NestedList a -> [a]
myFlatten1 (Elem x) = [x]
myFlatten1 (List (x:xs)) = myFlatten1 x ++ myFlatten1 (List xs)
myFlatten1 (List []) = []

-- Problem 8

myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:y:xs)
    | x == y    = myCompress (y:xs)
    | otherwise = x : myCompress (y:xs)

-- Problem 9

pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:xs)
    | x == head theHead = (x:theHead):theTail
    | otherwise         = [x]:theHead:theTail
    where thePack = pack xs
          theTail = tail thePack
          theHead = head thePack

-- Problem 10

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode [a] = [(1, a)]
encode (x:xs)
    | x == headValue = (headInt + 1, x) : tail theEncode
    | otherwise      = (1, x):theEncode
    where theEncode = encode xs
          headValue = snd (head theEncode)
          headInt   = fst (head theEncode)
