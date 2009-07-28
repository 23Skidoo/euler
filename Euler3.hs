module Euler3
    where

solve = let is_prime' x (y:ys) = if x == y
                                 then True
                                 else if x `mod` y == 0
                                      then False
                                      else is_prime' x ys
            is_prime x = is_prime' x [2..]
            primes = [x | x <- [2..], is_prime x == True ]

            largest_prime_factor' 1 _ f = f
            largest_prime_factor' x (y:ys) f = if x `mod` y == 0
                                               then largest_prime_factor' (x `div` y) (y:ys) y
                                               else largest_prime_factor' x ys f
            largest_prime_factor x = largest_prime_factor' x primes 1

         in largest_prime_factor 317584931803

-- Alternative take
solvealt = let largest_prime_factor' 1 f = f
               largest_prime_factor' x f = if x `mod` f == 0
                                           then largest_prime_factor' (x `div` f) f
                                           else largest_prime_factor' x (f+1)
               largest_prime_factor x = largest_prime_factor' x 2
           in largest_prime_factor 317584931803

main = putStrLn . show $ solve