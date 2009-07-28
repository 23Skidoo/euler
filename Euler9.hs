module Euler9
    where

type Triple a = (a,a,a)

triple_product :: (Num t) => Triple t -> t
triple_product (a,b,c) = a*b*c

pyt_triplets :: (Num a, Enum a) => [Triple a]
pyt_triplets = [(a,b,c) | a <- [200..600], b <- [200..600],
                c <- [200..600], a+b+c == 1000, c*c == a*a + b*b]

main :: IO ()
main = print . triple_product . head $ pyt_triplets
