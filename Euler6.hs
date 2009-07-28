module Euler6
    where

solve = let sum_of_squares = foldr1 (+) [x*x | x <- [1..100]]
            square_of_the_sum = (^2) $ foldr1 (+) [1..100]

        in square_of_the_sum - sum_of_squares

main = putStrLn . show $ solve