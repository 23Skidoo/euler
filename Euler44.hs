module Main
       where

import Common (pentagonal, isPentagonal)

solution :: [Int]
solution = [p_k - p_j | p_j <- pentagonal'
                       , p_k <- pentagonal'
                       , p_k > p_j
                       , isPentagonal (p_j + p_k)
                       , isPentagonal (p_k - p_j)]
  where
    n = 10000
    pentagonal' = take n pentagonal

main :: IO ()
main = print . head  . take 1 $ solution
