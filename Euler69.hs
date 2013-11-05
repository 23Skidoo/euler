module Main
       where

import Math.Sieve.Phi
import Data.Function (on)
import Data.List (maximumBy)

main :: IO ()
main = print . fst . maximumBy (compare `on` snd) $
       [(n,(fromIntegral n :: Double) / fromIntegral (phi s n :: Int))
       | n <- [1..hi]]
  where
    hi = 1000000 :: Int
    s  = sieve hi
