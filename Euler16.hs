module Main
    where

import Data.Char
import System

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> computeAnswer 1000
            (x:_) -> return (read x) >>= computeAnswer
    where
      computeAnswer n = print . sum $ map digitToInt (show $ (2^n))