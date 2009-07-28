module Main
    where

import Data.List

is_pandigital :: (Num a) => a -> a -> Bool
is_pandigital a b = digits == "123456789"
    where
      digits = sort $ show a ++ show b ++ show (a*b)

products :: [(Integer, Integer)]
products = [(a, b) | a <- [1..50], b <- [1..2000], is_pandigital a b]

main :: IO ()
main = print . sum . nub . map (\(a,b) -> a*b) $ products
