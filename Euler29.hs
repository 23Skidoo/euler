module Main
    where

import Common (uniq)
import Data.List

powers :: [Integer]
powers = uniq . sort $ [a^b | a <- [2..100], b <- [2..100]]

main :: IO ()
main = print . length $ powers
