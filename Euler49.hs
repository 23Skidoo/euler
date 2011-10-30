module Main
       where

import Data.Array
import qualified Data.List as List

import Common (digitsToNumber, numberToDigits, isPrime, primes, tabulate)

fourDigitPrimes :: [Int]
fourDigitPrimes = dropWhile (< 1000) primes

permutations :: Int -> [Int]
permutations = map digitsToNumber . List.permutations . numberToDigits

solution :: [(Int, Int, Int)]
solution = [(a,b,c) | a <- fourDigitPrimes
                    , a /= 1487
                    , b <- permutations a
                    , a < b && isPrime' b
                    , c <- permutations a
                    , a < c && b < c && (b - a) == (c - b) && isPrime' c
            ]
  where
    memo = (tabulate (1000, 10000) (\n -> isPrime n))
    isPrime' = (memo!)

main :: IO ()
main = putStrLn . (\(a,b,c) -> show a ++ show b ++ show c) . head $ solution
