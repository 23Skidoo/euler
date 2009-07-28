module Main
    where

import Data.Ratio

-- A very elegant solution for this by Wi:
-- [(10*x+y,10*y+z) | x <- [1..9], y <- [1..9], z <- [1..9],
--                         x /= y , (9*x*z) + (y*z) == (10*x*y)]
-- Explanation:
-- xy/yz = x/z
-- (10x + y)/(10y+z) = x/z
-- 9xz + yz = 10xy

simplify :: (Integral a) => a -> a -> Ratio a
simplify a b = if d == e
               then (c*d) % (if e*f == 0 then 1 else e*f)
               else 0 % 1
    where
      c = a `div` 10
      d = a `mod` 10
      e = b `div` 10
      f = b `mod` 10

fractions :: [Ratio Int]
fractions = [ a%b | a <- [10..99], b <- [10..99], a < b, simplify a b == a % b]

main :: IO ()
main = print . denominator . product $ fractions
