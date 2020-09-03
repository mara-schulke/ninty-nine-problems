-- https://sites.google.com/site/prologsite/prolog-problems/1

module Lists where

import Data.Maybe (fromJust)

-- 1.01 (*) Find the last element of a list.
last :: [a] -> Maybe a
last [] = Nothing
last (x:[]) = Just x
last (_:xs) = Lists.last xs

-- 1.02 (*) Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (_:xs) = lastButOne xs

-- 1.03 (*) Find the K'th element of a list.
nth :: Int ->  [a] -> Maybe a
nth target [] = Nothing
nth target (x:xs)
    | target < 0  = Nothing
    | target == 0 = Just x
    | otherwise   = Lists.nth (target - 1) xs

-- 1.04 (*) Find the number of elements of a list.
length :: [a] -> Int
length = foldl (\a x -> a + 1) 0

-- 1.05 (*) Reverse a list.
rev :: [a] -> [a]
rev = foldl (\ys x -> x : ys) []

-- make this generic!
-- 1.06 (*) Find out whether a list is a palindrome.
isPalindrome :: [Int] -> Bool
isPalindrome xs = take half xs == take half (rev xs)
                  where half = Lists.length xs `div` 2

-- 1.07 (**) Flatten a nested list structure.
flatten :: [[a]] -> [a]
flatten = foldl (++) []

-- make this generic!
-- 1.08 (**) Eliminate consecutive duplicates of list elements.
compress :: [Int] -> [Int]
compress = foldl (\ys x -> if x `elem` ys then ys else ys ++ [x]) []

-- 1.09 (**) Pack consecutive duplicates of list elements into sublists.
pack :: [Int] -> [[Int]]
pack = foldl pack' []

pack' :: [[Int]] -> Int -> [[Int]]
pack' [] x =  [[x]]
pack' ys x
    | x `elem` last = withoutLast ++ [last ++ [x]]
    | otherwise     = ys ++ [[x]]
    where last        = fromJust $ Lists.last ys
          len         = Lists.length ys
          withoutLast = take (len - 1) ys

-- pack :: [a] -> [Packed a]
-- pack = foldl pack' []

-- pack' ys x
--     | Single x `elem` ys      = (filter (\y -> y /= x) ys) ++ [Packed (2, x)]
--     | Packed (a, x) `elem` ys = 
--     | otherwise               = ys ++ [Single x]

-- 1.10 (*) Run-length encoding of a list.
encode :: [Char] -> [(Int, Char)]
encode = foldl encode' []

encode' :: [(Int, Char)] -> Char -> [(Int, Char)]
encode' [] x =  [(1, x)]
encode' ys x
    | x == snd last = withoutLast ++ [(fst last + 1, x)]
    | otherwise     = ys ++ [(1, x)]
    where last        = fromJust $ Lists.last ys
          len         = Lists.length ys
          withoutLast = take (len - 1) ys

-- 1.11 (*) Modified run-length encoding.
data Encoded = Single Char | Multiple Int Char deriving (Eq, Show)

intoEncoded :: (Int, Char) -> Encoded
intoEncoded (1, x) = Single x
intoEncoded (n, x)
    | n <= 0 = error "There shouldn't be tuples with 0 as occurence counter. Check your code."
    | n == 1 = Single x
    | n >= 1 = Multiple n x

fromEncoded :: Encoded -> (Int, Char)
fromEncoded e = case e of
    Single x     -> (1, x)
    Multiple n x -> (n, x)

encodem :: [Char] -> [Encoded]
encodem xs = map intoEncoded $ foldl encode' [] xs

-- 1.12 (**) Decode a run-length encoded list.
intoRaw :: (Int, Char) -> [Char]
intoRaw (n, x) = replicate n x

decode :: [Encoded] -> [Char]
decode xs = flatten $ map (intoRaw . fromEncoded) xs

-- 1.13 (**) Run-length encoding of a list (direct solution).
-- already implemented

-- 1.14 (*) Duplicate the elements of a list.
duplicate :: [a] -> [a]
duplicate = foldl (\ys x -> ys ++ [x, x]) []

-- 1.15 (**) Duplicate the elements of a list a given number of times.
duplicateTimes :: Int -> [a] -> [a]
duplicateTimes n = foldl (\ys x -> ys ++ replicate n x) []

-- 1.16 (**) Drop every N'th element from a list.
dropEvery :: Int -> [a] -> [a]
dropEvery n xs = map fst $ foldl (dropEvery' n) [] $ zip xs [1..]

dropEvery' :: Int -> [(a, Int)] -> (a, Int) -> [(a, Int)]
dropEvery' n ys x
    | (index == 0) && (n /= 1) = ys ++ [x]
    | index `mod` n == 0       = ys
    | otherwise                = ys ++ [x]
    where index = snd x


-- 1.17 (*) Split a list into two parts; the length of the first part is given.
split :: Int -> [a] -> Maybe ([a], [a])
split n xs
    | len >= n  = Just (take n xs, reverse $ take (len - n) $ reverse xs)
    | otherwise = Nothing
    where len   = Lists.length xs

-- 1.18 (**) Extract a slice from a list.
slice :: Int -> Int -> [a] -> Maybe [a]
slice a b xs
    | a + b > len = Nothing
    | otherwise   = Just $ reverse $ take b $ reverse $ take (a + b) xs
    where len     = Lists.length xs

-- 1.19 (**) Rotate a list N places to the left.
-- rotate :: Int -> [a] -> [a]

-- 1.20 (*) Remove the K'th element from a list.
-- removeAt :: Int -> [a] -> [a]

-- 1.21 (*) Insert an element at a given position into a list.
-- insertAt :: Int -> [a] -> [a]

-- 1.22 (*) Create a list containing all integers within a given range.
-- range :: Int -> Int -> [Int]

-- 1.23 (**) Extract a given number of randomly selected elements from a list.
-- randomSelect :: Int -> [a] -> IO [a]

-- 1.24 (*) Lotto: Draw N different random numbers from the set 1..M.
-- lotto :: Int -> Int -> IO [Int]

-- 1.25 (*) Generate a random permutation of the elements of a list.
-- shuffle :: [a] -> IO [a]

-- 1.26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
-- 
-- 1.27 (**) Group the elements of a set into disjoint subsets.
-- 
-- 1.28 (**) Sorting a list of lists according to length of sublists
-- sortByLength :: [[a]] -> [[a]]
-- sortByLengthFrequency :: [[a]] -> [[a]] -- [[3,3,3], [1], [1], [2, 2]] -> [[3,3,3], [2,2], [1], [1]]
