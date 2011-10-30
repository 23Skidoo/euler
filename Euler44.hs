module Main
       where

import Common (isPentagonal)

solution :: Int
solution = go 1 1
  where
    go k j | j >= k    = go (k+1) 1
           | otherwise = let d = (3*k^2 - k - 3*j^2 + j) `div` 2
                             s = (3*k^2 - k + 3*j^2 - j) `div` 2
                         in if isPentagonal d && isPentagonal s
                            then d
                            else go k (j+1)

main :: IO ()
main = print solution
