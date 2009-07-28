module Main
    where

import System

readNumbers :: String -> [Integer]
readNumbers = map read . lines

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> getContents >>= processInput
            (x:_) -> readFile x >>= processInput
    where
      processInput = print . sum . readNumbers