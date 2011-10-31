module Main
       where

import Data.Function
import Data.List
import Common (digitsToNumber, numberToDigits, isPrime, primes)

seqLength, numWildCards :: Int
seqLength = 8
numWildCards = 3

substitutions :: Int -> [Int]
substitutions p = [ digitsToNumber
                    . map (\d -> if d == wild then x else d) $ digits
                    | x <- [st..9] ]
  where
    st     = if head digits == wild then 1 else 0
    digits = numberToDigits p
    wild   = head . maximumBy (compare `on` length)
             . groupBy (==) . sort $ digits

check :: [Int] -> Int -> Bool
check ps param = go ps 0 0
  where
    go [] count _         = count == param
    go (n:ns) count total = if (total - count) > (10 - param)
                            then False
                            else if isPrime n
                                 then go ns (count + 1) (total + 1)
                                 else go ns count (total + 1)

solution :: [Int]
solution = [p | p <- primesN numWildCards, check (substitutions p) seqLength]

primesN :: Int -> [Int]
primesN n = filter (hasNdig) primes
  where
    hasNdig = any (== n) . map length
              . groupBy (==) . sort . numberToDigits

main :: IO ()
main = print . head $ solution
