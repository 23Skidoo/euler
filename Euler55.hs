module Main
    where

import Common (digitsToNumber, numberToDigits, isPalindromic)

is_lychrel :: (Integral a) => a -> Bool
is_lychrel x = is_lychrel' x 0
    where
      is_lychrel' _ 50 = True
      is_lychrel' n m  = let r = reverse_and_add n
                         in if isPalindromic r
                            then False
                            else is_lychrel' r (m+1)

      reverse_and_add n = n + (digitsToNumber. reverse . numberToDigits $ n)

-- Interesting: swapping Integer for Int here results in a massive space leak.
-- TOFIX: investigate.
lychrel :: [Integer]
lychrel = [x | x <- [1..], is_lychrel x]

main :: IO ()
main = print . length . takeWhile (< 10^4) $ lychrel
