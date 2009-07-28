module Main
    where

-- Easy as pie.

main :: IO ()
main = print . mod (sum [a^a | a <- [1..1000]]) $ 10^10
