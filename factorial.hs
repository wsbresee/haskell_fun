main = putStrLn (show (factorial 4))

factorial :: (Integral a)  => a -> a
factorial 0 = 1
factorial x = x * (factorial (x - 1))
