module Main
    where

-- This is not very speedy, but works thanks to memoization.

import Common (proper_divisors, tabulate)
import Data.Array

is_abundant :: (Integral a) => a -> Bool
is_abundant n = (sum . proper_divisors $ n) > n

is_abundant_m :: (Ix t, Integral t) => t -> Bool
is_abundant_m n = memo ! n
    where
      memo = tabulate (1,28123) is_abundant

abundant :: Integral a => [a]
abundant = 12 : filter is_abundant [13..]

is_sum_2_ab :: (Ix a, Integral a) => a -> Bool
is_sum_2_ab n = is_sum_2_ab' m_ab
    where
      m_ab = takeWhile (>0) [n-a | a <- abundant]

      is_sum_2_ab' [] = False
      is_sum_2_ab' (x:xs) = if is_abundant_m x
                            then True
                            else is_sum_2_ab' xs

not_sum_2_ab :: [Integer]
not_sum_2_ab = [1..23] ++ filter (not . is_sum_2_ab) [25..28123]

main :: IO ()
main = print . sum $ not_sum_2_ab
