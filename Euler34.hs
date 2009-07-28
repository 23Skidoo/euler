module Main
    where

import Data.Char

factorial :: (Num t, Enum t) => t -> t
factorial n = product [1..n]

factorialDigits :: (Show a) => a -> Int
factorialDigits n = sum . map factorial . map digitToInt $ (show n)

main :: IO ()
main = print . sum $ [n | n <- [100..100000], n == factorialDigits n]
