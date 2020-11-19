import System.Random

-- Problem 23

rndSelect xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

-- Problem 24

diffSelect x n = rndSelect [1..x] n

-- Problem 25

getRandomIndex :: Int -> IO [Int]
getRandomIndex i = do
    randomIndex <- rndSelect [1..i] 1
    let theHead = randomIndex
    return randomIndex

removeAtIndex :: [a] -> Int -> (a, [a])
removeAtIndex (x:xs) i
  | i == 1      = (x, xs)
  | otherwise   = ((fst theRest), (x : (snd theRest)))
      where
          theRest = removeAtIndex xs $ i - 1

rndPermu :: [a] -> IO [a]
rndPermu [] = do
    return []
rndPermu xs = do
    rand <- getRandomIndex (length xs)
    let theRand = head rand
    let result = removeAtIndex xs theRand
    secondHalf <- rndPermu $ snd result
    return $ (fst result) : secondHalf
