module Main
    where

import Common (binomial)

main :: IO ()
main = print . length $ [ b | n <- [23..100], r <- [1..n-1],
                                   let b = binomial n r, b > 10^6]
