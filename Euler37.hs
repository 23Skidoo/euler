module Main
    where

import Common (digits10, isPrime, primes)

isTruncatable :: (Integral a) => a -> Bool
isTruncatable x = (and . truncate_rl $ x) && (and . truncate_lr $ x)
    where
      truncate_rl n = [isPrime (n `mod` 10^k) | k <- [1..(digits10 n)-1]]
      truncate_lr n = [isPrime (n `div` 10^k) | k <- [1..(digits10 n)-1]]

main :: IO ()
main = print . sum . take 11 $ truncatable
    where
      truncatable = filter isTruncatable (dropWhile (< 10) primes)
