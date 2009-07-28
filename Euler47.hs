module Main
    where

import Data.List
import NumberTheory.Sieve.Factor
import Common (defactorize)

sum' :: [Int] -> Int
sum' = foldl1' (+)

distinct :: (Eq t) => t -> [t] -> Bool
distinct a xs = and [ x /= a | x <- xs ]

num_distinct :: (Eq a) => [a] -> [a] -> Int
num_distinct xs ys = sum' . map fromEnum . map (flip ($) ys) $ (map distinct xs)


two_distinct :: (Eq a) => [a] -> [a] -> Bool
two_distinct a b = num_distinct a b == 2 && num_distinct b a == 2

three_distinct :: (Eq a) => [a] -> [a] -> [a] -> Bool
three_distinct a b c = num_distinct a b == 3 && num_distinct b c == 3
                         && num_distinct c a == 3

four_distinct :: (Eq a) => [a] -> [a] -> [a] -> [a] -> Bool
four_distinct a b c d = num_distinct a b == 4 && num_distinct b c == 4
                          && num_distinct c d == 4 && num_distinct d a == 4

dropWhile2 :: (t -> t -> Bool) -> [t] -> [t]
dropWhile2 f (x0:x1:xs) = if f x0 x1 then x0:rest else rest
    where rest = dropWhile2 f (x1:xs)
dropWhile2 _ _ = error "Works only on infinite lists!"

dropWhile3 :: (t -> t -> t -> Bool) -> [t] -> [t]
dropWhile3 f (x0:x1:x2:xs) = if f x0 x1 x2 then x0:rest else rest
    where rest = dropWhile3 f (x1:x2:xs)
dropWhile3 _ _ = error "Works only on infinite lists!"

dropWhile4 :: (t -> t -> t -> t -> Bool) -> [t] -> [t]
dropWhile4 f (x0:x1:x2:x3:xs) = if f x0 x1 x2 x3 then x0:rest else rest
    where rest = dropWhile4 f (x1:x2:x3:xs)
dropWhile4 _ _ = error "Works only on infinite lists!"

main :: IO ()
main = print .
       defactorize . head . process_quadriples . map factorize $ [2..]
           where
             factorize = factor mySieve
             mySieve = sieve 1000000
             --process_pairs = dropWhile2 two_distinct
             --process_triples = dropWhile3 three_distinct
             process_quadriples = dropWhile4 four_distinct
