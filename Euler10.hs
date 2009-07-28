module Euler10
    where

import Common (primes)

primesBelowMillion = takeWhile (\p -> p <= 10^6) primes

solve = sum primesBelowMillion

main = putStrLn . show $ solve
