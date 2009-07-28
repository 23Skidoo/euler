module Main
    where

import System
import Data.Array.Unboxed as A


fltr = filter (\(x,y) -> 0 <= x && x <= 19 && 0 <= y && y < 19)
vert (x,y) = [(x+n,y) | n <- [0..3]]
horis (x,y) = [(x,y+n) | n <- [0..3]]
diag (x,y) = [(x+n,y+n) | n <-[0..3]]
diag2 (x,y) = [(x+n,y-n) | n <-[0..3]]

index_list_to_elements index_list ar = map (\x -> ar ! x) index_list
product_arr' lst ar = product $ index_list_to_elements lst ar
product_arr p arr = maximum $ map (\f -> product_arr' (fltr . f $ p) arr) [vert, horis, diag, diag2]

--solve :: A.UArray (Int, Int) Int -> Int
solve arr = maximum $ map (\i -> product_arr i arr) (indices arr)

-- Convert an n x n integer matrix from string representation to an UArray
readMatrix :: String -> A.UArray (Int, Int) Int
readMatrix str = let strtolist = map (map read) . map words . lines
                     lst = strtolist str
                     n = length lst
                     assocs = [((a,b), lst !! a !! b)
                                   | a <- [0..n-1], b <- [0..n-1]]
                 in
                   array ((0,0), (n-1, n-1)) assocs

main :: IO ()
main = do args <- getArgs
          case args of
            [] ->  getContents >>= processInput
            (x:_) -> readFile x >>= processInput
    where
      processInput = print . solve . readMatrix