module Main
    where

-- Fancy memoized solution.

import Data.Array (Array, (!))
import Common (numberToDigits, tabulate)

nextInChain :: Int -> Int
nextInChain = sum . map (^2) . numberToDigits

chain' :: Int -> Bool
chain' 1  = False
chain' 89 = True
chain' n  = chain' (nextInChain n)

memo :: Array Int Bool
memo = tabulate (1, 567) chain'

chain :: Int -> Bool
chain k = memo ! nextInChain k

main :: IO ()
main = print . length . filter (== True) . map chain $ [1..10^7-1]
