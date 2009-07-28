module Euler5
    where

solve = let is_divisible _ [] = True
            is_divisible num (x:xs) = num `mod` x == 0 && is_divisible num xs

            find_divisible' []     n _ = n
            find_divisible' (x:xs) n l = if is_divisible n_by_x l
                                         then find_divisible' (x:xs) n_by_x l
                                         else find_divisible' xs n l
                                             where n_by_x = (n `div` x)

            find_divisible l = find_divisible' l (foldl (*) 1 l) l

        in find_divisible [2..20]

-- Alternative take
solvealt = let gcd a 0 = a
               gcd a b = gcd b (a `mod` b)
               lcm a b = (a*b) `div` gcd a b

           in foldl1 lcm [1..20]

main = putStrLn $ solve