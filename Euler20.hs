module Main
    where

import Data.Char

fac n = product [2..n]

main = print . sum $ map digitToInt (show . fac $ 100)