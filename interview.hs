import Data.List

-- Reverse a list
myReverse :: [a] -> [a]
myReverse xs = foldl (\ acc x -> x:acc) [] xs

-- Check if a list is a palindrome
myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome xs = xs == myReverse xs

-- Count the number of vowels and consonants in a string
vowels = ['a', 'e', 'i', 'o', 'u']

calcVowelsAndConsonants :: [Char] -> (Int, Int)
calcVowelsAndConsonants xs = numElemFromFirstListAppearInSecond vowels xs

numElemFromFirstListAppearInSecond :: (Eq a) => [a] -> [a] -> (Int, Int)
numElemFromFirstListAppearInSecond xs1 xs2 = (matches, length xs2 - matches)
    where matches = foldl (\ acc x -> acc + countMatchesInList x xs2) 0 xs1

countMatchesInList :: (Eq a) => a -> [a] -> Int
countMatchesInList y xs =
    foldl (\ acc x -> if x == y then acc + 1 else acc) 0 xs

-- Anagrams
anagrams :: (Ord a) => [a] -> [a] -> Bool
anagrams x y = sort x == sort y
