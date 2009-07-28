module Main
    where

import Data.Char

-- Brute force.

sum_powers :: (Show a) => a -> Int
sum_powers n = sum [ a | a <- map ((^5) . digitToInt) (show n)]

as_sum_powers :: [Int]
as_sum_powers = [n | n <- [2..1000000], sum_powers n == n]

main :: IO ()
main = print . sum $ as_sum_powers
