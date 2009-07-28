module Main
    where

import Common (hexagonal, isPentagonal)

main :: IO ()
main = print . head $ [n | n <- drop 143 hexagonal, isPentagonal n]
