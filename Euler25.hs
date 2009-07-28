module Main
    where

fibs :: Num a => [a]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

numfibs :: (Num a, Enum a, Num b) => [(a, b)]
numfibs = zip [1..] fibs

first1000 :: (Num a, Enum a) => a
first1000 = fst . head $ dropWhile (\(_, n) -> n < 10^999) numfibs

main :: IO ()
main = print first1000
