module Main
    where

import Common (primes, isPrime)
import Data.List

limit :: Int
limit = 10^2

consecutivePrimeSums :: [(Int, Int)]
consecutivePrimeSums = concatMap consecutiveSumsF primeTailsF
    where
      primeTails = iterate tail primes
      primeTailsF = takeWhile ((< limit) . head) primeTails

      consecutiveSums l = map (\i -> (length i, sum i)) . drop 1 . inits $ l
      consecutiveSumsF l = takeWhile ((< limit) . snd) (consecutiveSums l)

-- Faster way to do it:
problem50 :: Int
problem50 = head . filter isPrime $ map sum allTails
    where
      allInits = takeWhile ((< limit) . sum) . inits $ primes
      allTails = concatMap tails . reverse $ allInits

main :: IO ()
main = print . maximum . filter (isPrime . snd)
       $ consecutivePrimeSums
