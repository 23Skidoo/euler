module Euler4
    where

-- We can assume that x has 6 digits
solve = let is_palindromic x = first == sixth && second == fifth && third == fourth
                where first = x `mod` 10
                      second = (x `mod` 100) `div` 10
                      third = (x `mod` 1000) `div` 100
                      fourth = (x `mod` 10000) `div` 1000
                      fifth = (x `mod` 100000) `div` 10000
                      sixth = x `div` 100000

            products = [a*b | a <- [901..999], b <- [901..999] ]

        in maximum (filter is_palindromic products)

main = putStrLn . show $ solve