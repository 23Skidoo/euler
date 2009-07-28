module Main
    where

import Common (numberToDigits, inputFileInteract)
import Data.List
import qualified Data.IntSet as I
import qualified Data.Map as M

-- [3,1,7] -> [(3,[]),(1,[3]),(7,[3,1])]
before :: [a] -> [(a, [a])]
before xs = zipWith (\a b -> (a,b)) xs (init . inits $ xs)

-- Solution is straightforward:
-- Build a map from numbers to the preceding number set and sort
-- by the cardinality of these sets.
analyse :: [(Int, [Int])] -> Int
analyse l = getMax . analyse' l $ M.empty
    where
      analyse' [] m = m
      analyse' ((x,xs):ys) m
          = analyse' ys (M.insertWith I.union x (I.fromList xs) m)

      getMax m = foldl1 (\a b -> a*10 + b) . map fst .
                 sortBy (\(_,a) (_,b) -> compare a b) .
                 M.toList . M.map (\a -> I.size a) $ m

main :: IO ()
main = inputFileInteract processInput
    where
      processInput = analyse . concatMap (before . numberToDigits) .
                     map read . words
