module Main
    where

import Data.List

-- Partition the list L into (length L) tuples (x, rest) for each x
part :: (Eq t) => [t] -> [(t, [t])]
part xs = [(e, delete e xs) | e <- xs]

lexperms :: String -> [String]
lexperms [] = []
lexperms [x] = [[x]]
lexperms [x,y] = [[x,y],[y,x]]
lexperms xs = concatMap (\(n,r) -> map (n:) (lexperms r)) (part xs)

millionthPerm :: Integer
millionthPerm = read . head . drop 999999 $ (lexperms "0123456789")

main :: IO ()
main = print millionthPerm
