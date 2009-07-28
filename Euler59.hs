module Main
    where

import Common
import Data.Bits
import Data.List
import Data.Function

type Key = (Int, Int, Int)

dexor :: Key -> [Int] -> [Int]
dexor _ []                      = []
dexor (a,_,_) [a1]              = [a`xor`a1]
dexor (a,b,_) [a1,b1]           = [a `xor` a1, b `xor` b1]
dexor abc@(a,b,c) (a1:b1:c1:as) = (a `xor` a1):(b `xor` b1):(c `xor` c1)
                                  :dexor abc as

keys :: [Key]
keys = [(a,b,c) | a <- lowercase_ascii, b <- lowercase_ascii,
        c <- lowercase_ascii]
    where
      lowercase_ascii = [97..122]

analyse :: [Int] -> Int
analyse l = sum . dexor key $ l
    where
      key = fst . maximumBy (compare `on` snd) $
            [ (abc, countTHE . dex $ l)
              | abc <- keys, let dex = dexor abc]

      countTHE [] = 0
      countTHE (116:104:101:xs) = 1 + countTHE xs
      countTHE (_:xs) = countTHE xs

main :: IO ()
main = inputFileInteract processInput
    where
      processInput = analyse . map read . words
