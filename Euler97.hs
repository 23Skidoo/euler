module Main
    where

theprime :: Num a => a
theprime = 28433*2^7830457+1

lastd :: String
lastd = drop 2357197 . show $ theprime

main :: IO ()
main = print $ ((read lastd)::Integer)
