module Main
    where

import Data.List

sameDigits :: (Show a) => a -> a -> Bool
sameDigits a b = repr a == repr b
    where
      repr = sort . show

sameDigitsUpTo6x :: (Num a, Enum a) => a -> Bool
sameDigitsUpTo6x x = and $ zipWith sameDigits m (tail m)
    where
      m = [k*x | k <- [2..6]]

main :: IO ()
main = print $ head [ x | x <- [2..], sameDigitsUpTo6x x ]
