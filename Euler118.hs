module Main
    where

-- This sucks | and needs to be rewritten.

import Data.Array
import Data.List
import qualified Data.Set as S
import Common (digitsToNumber, isPrime, permutations, tabulate)

allConcats :: (Num a, Enum a) => [Int] -> [[a]]
allConcats = makeNumbers . ac
    where
      combine :: t -> [[t]] -> [[[t]]]
      combine x = (\l -> [([x]:l),((x:head l):(tail l))])

      ac :: [a] -> [[[a]]]
      ac [] = []
      ac [x] = [[[x]]]
      ac (x:xs) = concatMap (combine x) (ac xs)

      makeNumbers :: (Num a, Enum a) => [[[Int]]] -> [[a]]
      makeNumbers = map (map digitsToNumber)

memo :: Array Integer Bool
memo = tabulate (1, 7654321) isPrime

isPrimeMemoized :: Integer -> Bool
isPrimeMemoized n = memo ! n

isListPrime :: (Integral t) => [t] -> Bool
isListPrime []     = True
isListPrime (x:xs) = if x == 2 || odd x && isPrime x
                     then isListPrime xs
                     else False

main :: IO ()
main = print . S.size . S.fromList . filter isListPrime . map sort .
       concatMap allConcats $ (permutations [1..9])

