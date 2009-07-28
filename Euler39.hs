module Main
    where

import Data.List

num_solutions :: Int -> Int
num_solutions x = (length . solutions $ x) `div` 2
    where
      solutions :: Int -> [(Int, Int, Int)]
      solutions p = [(a,b,c) | c <- [1..p-2], a <- [1..p-c-1],
                     let b = p-c-a, c^(2::Int) == a^(2::Int) + b^(2::Int)]

main :: IO ()
main = print . fst . maximumBy (\(_, np1) (_, np2) -> compare np1 np2)
       $ [(p, num_solutions p) | p <- [10..1000]]
