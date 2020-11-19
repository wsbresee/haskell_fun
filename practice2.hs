theList = [1,2,3,4,5]

myLast :: [a] -> a
myLast [] = error "An empty list has no last value"
myLast [x] = x
myLast (x:xs) = myLast xs

secondToLast :: [a] -> a
secondToLast [] = error "this is an empty list"
secondToLast [x] = error "this list only has one element"
secondToLast (x:xs) =
    if length xs == 1 then x
    else secondToLast xs

elementAt :: (Num b, Ord b) => [a] -> b -> a
elementAt [] _ = error "This is an empty list"
elementAt [x] y
    | y == 0    = x
    | otherwise = error "index out of bounds"
elementAt [x:xs] y
    | y == 0    = x
    | y > 0     = elementAt xs (y - 1)
    | otherwise = error "negative index"
