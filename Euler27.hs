module Main
    where

import Common (isPrime)
import Data.List (maximumBy)

formula :: (Num a, Enum a) => a -> a -> [a]
formula a b = [n^2 + a*n + b | n <- [0..]]

primes_generated :: [Integer] -> Int
primes_generated = length . takeWhile isPrime

formulas :: [(Integer, Int)]
formulas = [(a*b, primes_generated (formula a b))
            | a <- [-999..999], b <- [-999..999]]

main :: IO ()
main = print . fst .
       maximumBy (\(_, pg1) (_, pg2) -> compare pg1 pg2) $ formulas
