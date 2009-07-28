module Main
    where

import Data.Char

main :: IO ()
main = print . product . map digitToInt . take 6
       $ [irr !! (10^n-1) | n <- [0..]]
    where
      irr = concat [ show d | d <- [1..] ]
