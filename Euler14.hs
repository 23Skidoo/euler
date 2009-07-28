module Main
    where

import Data.List
import Data.Ord

next_hailstone n | even n = n `div` 2
                 | otherwise = 3*n+1

gen_next_hailstone n
    = if nh == 1
      then Nothing
      else Just (nh, nh)
          where nh = next_hailstone n

hailstone n = unfoldr gen_next_hailstone n

hailstone_seqs = map hailstone [1..1000000]

zip_hailstone = zip [1..1000000] hailstone_seqs

max_hailstone = maximumBy (comparing (length . snd)) zip_hailstone

main = print . fst $ max_hailstone
