module Main
    where

import Common (permutations)

import Data.List

pandigitals :: [[Int]]
pandigitals = filter ((/=) 0 . head) . permutations $ [0,1,2,3,4,5,6,7,8,9]

substring_divisible :: [Int] -> Bool
substring_divisible s = substring_divisible' (tail s) [2,3,5,7,11,13,17]
    where
      substring_divisible' (_:_:[]) [] = True
      substring_divisible' (d1:d2:d3:ds) (x:xs) =
          if (d1*100 + d2*10 + d3) `mod` x == 0
          then substring_divisible' (d2:d3:ds) xs
          else False
      substring_divisible' _ _ = error "we shouldn't get here!"

main :: IO ()
main = print . sum $ [toNumber n | n <- pandigitals, substring_divisible n ]
    where
      toNumber :: [Int] -> Integer
      toNumber = foldl' (\a b -> a * 10 + (toInteger b)) 0
