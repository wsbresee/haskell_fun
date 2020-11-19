import System.Random
import Debug.Trace

-- Problem 21

split2 :: [a] -> Int -> ([a],[a])
split2 [] _ = ([],[])
split2 (x:xs) i
    | i >= 1     = ((x : fst result) , (snd result))
    | otherwise  = ((fst result) , (x : snd result))
    where
        result = split2 xs (i - 1)

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = (fst splits) ++ [x] ++ (snd splits)
    where
        splits = split2 xs i

-- Problem 22

range :: Int -> Int -> [Int]
range i j
  | i == j      = [j]
  | i < j       = i : range (i + 1) j
  | i > j       = i : range (i - 1) j

-- Problem 23

rndSelect xs n = do
    gen <- getStdGen
    return $ take n [xs !! x | x <- randomRs (0, (length xs) - 1) gen]

-- Problem 24

diffSelect n i = rndSelect [1..i] n

-- Problem 25

rndPermu :: [a] -> IO [a]
rndPermu []     = return []
rndPermu (x:xs) = do
    g <- getStdGen
    let rand = fst $ randomR (0, length xs) g
    rest <- rndPermu xs
    return $ let (firstHalf, secondHalf) = split2 rest rand
             in firstHalf ++ (x : secondHalf)

-- Problem 26

combinations :: Int -> [a] -> [[a]]
combinations d (x:xs)
  | length xs == 0 = [[x]]
  | d == 1         = [x] : combinations 1 xs
  | otherwise      = combiHelper d 1 (x:xs)

combiHelper :: Int -> Int -> [a] -> [[a]]
combiHelper d i xs
  | i >= (length xs) = perms
  | otherwise        = perms ++ combiHelper d (i + 1) xs
    where
        removed = removeAt i xs
        perms = map ((fst removed):) $ combinations (d - 1) (snd removed)

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

-- Problem 27

group :: [Int] -> [a] -> [[a]]
group (i:is) xs
  | is == []    = groupHelper i xs
  | otherwise   = (groupHelper i xs) ++ group is xs

groupHelper :: Int -> [a] -> [[a]]
groupHelper 1 [x] = [[x]]
groupHelper i [x] = []
groupHelper i (x:xs)
  | i == 1    = [x] : (groupHelper i xs)
  | otherwise = (map (x:) (groupHelper (i - 1) xs)) ++ groupHelper i xs

-- Problem 28

lfsort :: [[a]] -> [[a]]
lfsort xs = lfsortHelper xs $ sortLengths (countLengths xs)

lfsortHelper :: [[a]] -> [(Int, Int)] -> [[a]]
lfsortHelper _ [] = []
lfsortHelper xs (l:ls) = ((lfsortHelpHelp xs (fst l)) ++ (lfsortHelper xs ls))

lfsortHelpHelp :: [[a]] -> Int -> [[a]]
lfsortHelpHelp xs l
  | (length xs) == 0        = []
  | (length (head xs)) == l = ((head xs) : (lfsortHelpHelp (tail xs) l))
  | otherwise               = lfsortHelpHelp (tail xs) l

sortLengths :: [(Int, Int)] -> [(Int, Int)]
sortLengths [] = []
sortLengths [x] = [x]
sortLengths (x:xs) = (sortLengths lower) ++ [x] ++ (sortLengths higher)
    where
        parted = sortHelper xs (snd x)
        lower  = fst parted
        higher = snd parted


sortHelper :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
sortHelper [] _ = ([], [])
sortHelper (x:xs) p
  | (snd x) < p = ((x : (fst rest)), (snd rest))
  | otherwise    = ((fst rest), (x: (snd rest)))
    where
        rest = sortHelper xs p

countLengths :: [[a]] -> [(Int, Int)]
countLengths [x] = addLength (length x) []
countLengths (x:xs) = addLength (length x) (countLengths xs)

addLength :: Int -> [(Int, Int)] -> [(Int, Int)]
addLength l [] = [((l), 1)]
addLength l (x:xs)
  | l == (fst x) = ((fst x), ((snd x) + 1)) : xs
  | otherwise    = x : (addLength l xs)
