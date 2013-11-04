module Main
       where

import Common (digits10)
import Data.List (unfoldr)
import Data.Ratio

expansions :: [Rational]
expansions = map (1+) . unfoldr mkNext $ (1 % 2)
  where
    mkNext r = let r' = 1 / (2 + r) in Just (r,r')

main :: IO ()
main = print . length . filter (\r -> digNum r > digDem r)
       . take 1000 $ expansions
  where
    digNum = digits10 . numerator
    digDem = digits10 . denominator
