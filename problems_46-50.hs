import Control.Monad (replicateM)

-- Problem 46

not' :: Bool -> Bool
not' False = True
not' True  = False

and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

and' True True = True
and' _ _       = False

or' True _ = True
or' _ True = True
or' _ _    = False

nand' a b = not' $ and' a b

nor' a b = not' $ or' a b

xor' True False = True
xor' False True = True
xor' _ _        = False

impl' a b = or' (not' a) b

equ' True True   = True
equ' False False = True
equ' _ _         = False

table' :: (Bool -> Bool -> Bool) -> IO ()
table' f = putStrLn $ concatMap ( ++ "\n" )
           [show a ++ show b ++ show (f a b)
             | a <- [True, False], b <- [True, False]]

-- Problem 47

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

-- Problem 48

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

-- Problem 49

gray :: Int -> [String]
gray 0 = [""]
gray n = map ('0':) gray' ++ map ('1':) gray'
    where gray' = gray (n - 1)

-- Problem 50

data HTree a = Leaf a | Branch (HTree a) (HTree a)
    deriving Show

sortNodes :: [(a, Int)] -> [(a, Int)]
sortNodes []     = []
sortNodes (p:xs) = (sortNodes lesser) ++ [p] ++ (sortNodes greater)
    where
        lesser  = filter ((\x p -> (snd x) >= (snd p)) p) xs
        greater = filter ((\x p -> (snd x) < (snd p)) p) xs

push :: (HTree a, Int) -> [(HTree a, Int)] -> [(HTree a, Int)]
push x1 []              = [x1]
push x1 (x2:xs)
  | (snd x1) < (snd x2) = x1:x2:xs
  | otherwise           = x2:(push x1 xs)

formatList :: [(a, Int)] -> [(HTree a, Int)]
formatList xs = sortNodes formattedNodes
    where
        formattedNodes = map (\x -> (Leaf (fst x), (snd x))) xs

buildTree :: [(HTree a, Int)] -> [(HTree a, Int)]
buildTree [x] = [x]
buildTree (x1:x2:xs) = buildTree $ push newNode xs
    where
        newFreq = (snd x1) + (snd x2)
        newNode = ((Branch (fst x1) (fst x2)), newFreq)

huffman :: [(a, Int)] -> [(a, String)]
huffman xs = huffmanHelper $ fst $ head $ buildTree (formatList xs)

huffmanHelper :: (HTree a) -> [(a, String)]
huffmanHelper (Leaf x) = [(x, "")]
huffmanHelper (Branch x y) = leftSide ++ rightSide
    where
        leftSide  = map (\x -> (fst x, "0"++(snd x))) (huffmanHelper x)
        rightSide = map (\x -> (fst x, "1"++(snd x))) (huffmanHelper y)
