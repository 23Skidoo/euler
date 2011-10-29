module Main
    where

import Common (isPrime)

oddComposites :: [Int]
oddComposites = [ n | n <- [3,5..], not . isPrime $ n]

twiceSquares :: [Int]
twiceSquares = [ 2*n*n | n <- [1..]]

contradictsGolbach :: Int -> Bool
contradictsGolbach n =
    null [ (s,p) | s <- takeWhile (\s -> n - s >= 2) twiceSquares,
                        let p = n - s, isPrime p]

goldbach :: [Int]
goldbach = [ n | n <- oddComposites, not . contradictsGolbach $ n ]

falsifyGoldbach :: [Int]
falsifyGoldbach = [ n | n <- oddComposites, contradictsGolbach n ]

main :: IO ()
main = print . head $ falsifyGoldbach
