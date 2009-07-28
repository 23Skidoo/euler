module Main
    where

import Data.Char (ord)
import Common (inputFileInteract, isTriangle, readWords)

main :: IO ()
main = inputFileInteract processInput
    where
      processInput = length . filter isTriangle .
                     map (sum . map (subtract 64 . ord)) . readWords
