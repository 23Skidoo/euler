module Main
    where

import Data.List
import Common (digitsToNumber, isPrime, permutations)

candidates :: [Integer]
candidates = [ digitsToNumber digits | n <- [7,6..1],
               digits <- permutations [n,n-1..1]]

main :: IO ()
main = print . head . filter isPrime $ candidates
