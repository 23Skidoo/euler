module Main
    where

numDigits :: Integer -> Int
numDigits = length . show

powers :: Integer -> [Int]
powers k = takeWhile (\n -> numDigits (k^n) == n) [1..]

main :: IO ()
main = print . length . concatMap powers $ [1..9]
