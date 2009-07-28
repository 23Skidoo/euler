module Main
    where

import Common (triangle, factorize, num_divisors)

triangle_divisors = map num_divisors triangle

with_divisors = zip triangle triangle_divisors

with_over_500_divisors = dropWhile (\(_,y) -> y <= 500) with_divisors

first_num = fst . head $ with_over_500_divisors

main :: IO ()
main = print first_num
