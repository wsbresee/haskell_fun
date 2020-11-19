data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Problem 55

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch 'x' left right | i     <- [q..q+r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - 1 - i)]

-- Problem 56

tMirror :: Tree a -> Tree a -> Bool
tMirror Empty          Empty          = True
tMirror (Branch _ a b) (Branch _ x y) = (tMirror a y) && (tMirror b x)
tMirror _              _              = False

tSym :: Tree a -> Bool
tSym (Branch _ l r) = tMirror l r

-- Problem 57

addToTree :: Tree Int -> Int -> Tree Int
addToTree Empty i = (Branch i Empty Empty)
addToTree (Branch x l r) i
  | i <= x    = (Branch x (addToTree l i) r)
  | otherwise = (Branch x l (addToTree r i))

constructBSN :: [Int] -> Tree Int
constructBSN is = foldl addToTree Empty is

-- Problem 58

symCbalTrees :: Int -> [Tree Char]
symCbalTrees i = filter tSym $ cbalTree i

-- Problem 59

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree e 1 = [Branch e Empty Empty]
hbalTree e h = [Branch e left right | i     <- [h-1, h-2],
                                      j     <- [h-1, h-2],
                                      left  <- hbalTree e i,
                                      right <- hbalTree e j]

-- Problem 60

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + (countNodes l) + (countNodes r)

minNodes :: Int -> Int
minNodes h = countNodes $ minNodes' h
    where
        minNodes' h
          | h == 0    = Empty
          | h == 1    = Branch 'x' Empty Empty
          | otherwise = Branch 'x' (minNodes' (h-1)) (minNodes' (h-2))

maxHeight :: Int -> Int
maxHeight n = maxHeight' n 1
    where
        maxHeight' n h
          | minNodes h > n = h - 1
          | otherwise      = maxHeight' n (h + 1)

maxNodes :: Int -> Int
maxNodes h = 2^h - 1

minHeight :: Int -> Int
minHeight n = minHeight' n 1
    where
        minHeight' n h
          | maxNodes h < n = minHeight' n (h + 1)
          | otherwise      = h

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = filter ((\n y -> n == (countNodes y)) n) trees
    where
        trees = foldl1 (++) [hbalTree x h | h <- [(minHeight n)..(maxHeight n)]]
