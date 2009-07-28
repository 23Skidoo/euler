module Main
    where

import Common (inputFileInteract, readWords)
import Data.Char
import Data.List

nameValue :: String -> Int
nameValue = sum . map (subtract 64 . ord)

nameScores :: [String] -> [Int]
nameScores l = zipWith (*) [1..] (map nameValue l)

main :: IO ()
main = inputFileInteract processInput
    where
      processInput = sum . nameScores . sort . readWords
