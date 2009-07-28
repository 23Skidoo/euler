module Main
    where

import Common (primes)

-- Brute-forceish solution, but relatively speedy (after a small optimization).
-- Better ways to do this:

-- the period of a number n's reciprocal is the smallest k such that
-- n divides 10^k - 1

-- http://mathworld.wolfram.com/FullReptendPrime.html

import Data.List
import Data.Maybe

isTwicePrefixOf :: (Eq a) => [a] -> [a] -> Bool
isTwicePrefixOf xs ys = let lxs = length xs
                        in isPrefixOf xs ys && isPrefixOf xs (drop lxs ys)

digits :: (Integral t) => t -> [t]
digits n = [10^k `div` n `mod` 10 | k <- [1..] ]

is_finite :: (Num t) => [t] -> Bool
is_finite xs = isPrefixOf [0,0,0,0,0,0,0] (drop 15 xs)

len_recurring :: (Integral a) => a -> Int
len_recurring n = if is_finite (digits n)
                  then 0
                  else foldr1 max skips
    where
      skips = [len_recurring' [head ds] (tail ds)
               | ds <- [ drop m . digits $ n | m <- [0..3]]]

      len_recurring' :: (Eq a) => [a] -> [a] -> Int
      len_recurring' xxs yys@(y:ys) = if isTwicePrefixOf xxs yys
                                      then length xxs
                                      else if length xxs > 1000
                                           then 0
                                           else len_recurring' (xxs ++ [y]) ys
      len_recurring' _ _            = error "Argument error!"

main :: IO ()
main = print . ((!!) primes) . fromJust . elemIndex max_rec $ lengths_rec
    where
      lengths_rec = map len_recurring (takeWhile (< 1000) primes)
      max_rec = foldr1 max $ lengths_rec
