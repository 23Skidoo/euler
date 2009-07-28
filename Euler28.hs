module Main
    where

import Data.List (replicate)

spiral :: [Integer]
spiral = 1 : zipWith (+) spiral (concatMap (replicate 4) [2,4..])

main :: IO ()
main = print . sum . takeWhile (<= 1001*1001) $ spiral
