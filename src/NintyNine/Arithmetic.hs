-- https://sites.google.com/site/prologsite/prolog-problems/2

module Arithmetic where

import Data.List (find)
import Data.Maybe (fromJust)

-- 2.01 (**) Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime n
    | n <= 3         = n > 1
    | n `mod` 2 == 0 = False
    | n `mod` 3 == 0 = False
    | otherwise      = isPrime' n 5

isPrime' :: Int -> Int -> Bool
isPrime' n i
    | i * i > n            = True
    | n `mod` i == 0       = False
    | n `mod` (i + 2) == 0 = False
    | otherwise            = isPrime' n (i + 6)

-- 2.02 (**) Determine the prime factors of a given positive integer.
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = [factor] ++ primeFactors (n `div` factor)
    where primes      = [x | x <- [2..n + 1], isPrime x]
          factorize x = fromJust $ find (\p -> x `mod` p == 0) primes
          factor      = factorize n

-- 2.03 (**) Determine the prime factors of a given positive integer (2).
-- Lists.encode could be used here, if it was generic
-- Possible implementation:
--
-- primeFactorsEncoded :: Int -> [(Int, Int)]
-- primeFactorsEncoded :: encode $ primeFactors

-- 2.04 (*) A list of prime numbers.
primesBetween :: Int -> Int -> [Int]
primesBetween f t = [x | x <- [f..t], isPrime x]

-- 2.05 (**) Goldbach's conjecture.
goldbachsConjecture :: Int -> Maybe (Int, Int)
goldbachsConjecture 2 = Nothing
goldbachsConjecture n
    | n `mod` 2 /= 0  = Nothing
    | otherwise       = Just (res, (n - res))
        where primes   = [x | x <- [2..n + 1], isPrime x]
              res      = fromJust $ find (\p -> exists $ find (\r -> (n - p == r)) primes) primes
              exists x = case x of
                             Just _  -> True
                             Nothing -> False

-- 2.06 (**) A list of Goldbach compositions.
-- 2.07 (**) Determine the greatest common divisor of two positive integer numbers.
-- 2.08 (*) Determine whether two positive integer numbers are coprime.
-- 2.09 (**) Calculate Euler's totient function phi(m).
-- 2.10 (**) Calculate Euler's totient function phi(m) (2).
-- 2.11 (*) Compare the two methods of calculating Euler's totient function
