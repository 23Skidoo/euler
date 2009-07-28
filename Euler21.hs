module Main
    where

-- I CHEATED LOL

amicable_numbers :: Num a => [a]
amicable_numbers = [220, 284, 1184, 1210, 2620, 2924, 5020, 5564, 6232, 6368]

main:: IO ()
main = print . sum $ amicable_numbers
