module Main
    where

import Data.List
import Data.Char

digital_sum :: (Show a) => a -> Int
digital_sum n = sum . map digitToInt . show $ n

main :: IO ()
main = print . maximumBy (\(_,_,a) (_,_,b) -> compare a b) $
       [(a,b, digital_sum (a^b)) | a <- [1..99], b <- [2..99]]
