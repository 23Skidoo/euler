module Main
    where

-- Can be made shorter by using showIntAtBase.
-- TODO: look at Numeric and Char modules.

is_palindromic :: (Eq a) => [a] -> Bool
is_palindromic str = str == (reverse str)

is_palindromic_both_bases :: (Integral a) => a -> Bool
is_palindromic_both_bases n = is_palindromic (show n) &&
                              is_palindromic (binary_show n)

binary_show :: (Integral a) => a -> [Char]
binary_show k = reverse . binary_show' $ k
    where
      binary_show' 0 = ""
      binary_show' n = dig : binary_show' (n `div` 2)
          where
            dig = if n `mod` 2 == 0 then '0' else '1'

main :: IO ()
main = print . sum $ [n | n <- [1..999999], is_palindromic_both_bases n]
