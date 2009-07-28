module Main
    where

import Common (primes, isPrime)

rotations :: Int -> [Int]
rotations n = n : rotate cycles n
    where
      cycles = subtract 1 . length . show $ n

      rotate 0 _ = []
      rotate k m = let rotation = (m `div` 10) + (m `mod` 10) * 10^cycles
                   in rotation : rotate (k-1) rotation

isCircularPrime :: Int -> Bool
isCircularPrime n = oddDigits n && (and . map isPrime $ rotations n)
    where
      oddDigits p = all (\x -> x `elem` ['1','3','7','9']) . show $ p

main :: IO ()
main = print . length $ 2:3:5:7:[n | n <- takeWhile (< (10^6)) (drop 4 primes),
                                          isCircularPrime n]
