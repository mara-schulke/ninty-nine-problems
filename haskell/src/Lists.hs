-- https://sites.google.com/site/prologsite/prolog-problems/1

module Lists where

-- 1.01 (*) Find the last element of a list.
last :: [a] -> a
last (x:[]) = x
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
length xs = length' 0 xs

length' :: Int -> [a] -> Int
length' a [] = a
length' a (x:xs) = length' (a + 1) xs

-- 1.05 (*) Reverse a list.
-- rev :: [a] -> Int

-- 1.06 (*) Find out whether a list is a palindrome.
-- isPalindrome :: [a] -> Bool

-- 1.07 (**) Flatten a nested list structure.
-- flatten :: [[a]] -> [a]

-- 1.08 (**) Eliminate consecutive duplicates of list elements.
-- compress :: [a] -> [a]

-- 1.09 (**) Pack consecutive duplicates of list elements into sublists.
-- pack :: [a] -> [a]

-- 1.10 (*) Run-length encoding of a list.
-- encode :: [a] -> [(Int, a)]

-- 1.11 (*) Modified run-length encoding.
-- -- how?

-- 1.12 (**) Decode a run-length encoded list.
-- decode :: [(Int, a)] -> a

-- 1.13 (**) Run-length encoding of a list (direct solution).
-- -- rl_encode :: [(Int, a)] -> a

-- 1.14 (*) Duplicate the elements of a list.
-- dublicate :: [a] -> [a]

-- 1.15 (**) Duplicate the elements of a list a given number of times.
-- dublicateTimes :: Int -> [a] -> [a]

-- 1.16 (**) Drop every N'th element from a list.
-- dropNth :: Int -> [a] -> [a]

-- 1.17 (*) Split a list into two parts; the length of the first part is given.
-- split :: Int -> [a] -> [[a]]

-- 1.18 (**) Extract a slice from a list.
-- slice :: Int -> Int -> [a] -> [a]

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