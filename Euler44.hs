module Main
       where

import qualified Data.IntSet as IntSet

gen_pentagonal :: [Int]
gen_pentagonal = [ n*(3*n - 1) `div` 2 | n <- [1..] ]

solution :: [Int]
solution = [p_k - p_j | p_j <- pentagonal
                       , p_k <- pentagonal
                       , p_k > p_j
                       , is_pentagonal (p_j + p_k)
                       , is_pentagonal (p_k - p_j)]
  where
    n = 10000
    pentagonal = take n gen_pentagonal
    pentagonal_set = IntSet.fromList $ take n gen_pentagonal
    is_pentagonal m = m `IntSet.member` pentagonal_set

main :: IO ()
main = print . head  . take 1 $ solution
