module Main
    where

import Data.List

isPandigital :: (Show a) => a -> Bool
isPandigital a = digits_a == "123456789"
    where digits_a = sort . show $ a

concat_product :: (Read a, Num a1, Enum a1) => a1 -> a1 -> a
concat_product k n = read . concat . map show . map (k*) $ [1..n]

concat_products :: [Integer]
concat_products = [ m | k <- [9000..10000],
                    let m = concat_product k 2,
                    isPandigital m]

main :: IO ()
main = print . last $ concat_products
