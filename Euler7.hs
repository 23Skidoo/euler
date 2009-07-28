module Euler7
    where

import Common (nthPrime)

-- Find the 10001st prime

solve :: Integer
solve = nthPrime 10001

main :: IO ()
main = putStrLn . show $ solve
