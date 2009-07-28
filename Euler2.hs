module Euler2
    where

solve = let fibonacci = 1:1:[ x+y | (x, y) <- zip fibonacci (tail fibonacci) ]
            take_even_lt_mil (x:xs) = if x > 1000000
                                      then []
                                      else if x `mod` 2 == 0
                                           then x:take_even_lt_mil xs
                                           else take_even_lt_mil xs
            fibonacci_even_lt_mil = take_even_lt_mil fibonacci
        in sum fibonacci_even_lt_mil

main = putStrLn . show $ solve