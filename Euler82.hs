module Main
       where

import Control.Monad (forM_)
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.List (foldl')
import Data.List.Split (wordsBy)

import Common (inputFileInteract)

data State = State { curCol   :: !Int
                   , curState :: !(Array Int Int)
                   } deriving (Eq, Show)

initialState :: State
initialState = State { curCol   = -1
                     , curState = listArray (1,0) []
                     }

main :: IO ()
main = inputFileInteract (solve . parse)

parse :: String -> Array (Int,Int) Int
parse f = listArray ((0,0),(w,w)) lst
  where
    lst = map read . wordsBy (\c -> c == ',' || c == '\n') $ f
    w   = count ',' . takeWhile (/= '\n') $ f

solve :: Array (Int,Int) Int -> Int
solve inparr = go initialState
  where
    w     = fst . snd . bounds $ inparr
    go st = let curCol'   = curCol st + 1
                curState' = updateState curCol' (curState st)
            in if curCol' == w
               then extractAnswer curState'
               else go (State curCol' curState')

    extractAnswer :: Array Int Int -> Int
    extractAnswer = minimum . elems

    updateState :: Int -> Array Int Int -> Array Int Int
    updateState col state = runSTArray $ do
      state' <- newArray (0,w) 0
      if col == 0
        -- Column 0: just copy the values from the input array.
        then forM_ [0..w] $ \i -> writeArray state' i (inparr ! (i,col))
        -- Column N: calculate the value for each cell.
        else do forM_ [0..w] $ \i -> writeArray state' i (calcVal state i col)

      return state'

    calcVal :: Array Int Int -> Int -> Int -> Int
    calcVal state row col = min (goUp   (row - 1) own initial)
                                (goDown (row + 1) own initial)
      where
        initial = state ! row + own
        own     = inparr ! (row, col)

        updateMinLen i curLen minLen = (curLen', minLen')
          where
            curLen' = curLen + inparr ! (i, col)
            minLen' = min minLen (curLen' + state ! i)

        goUp i curLen minLen
          | i < 0           = minLen
          | curLen > minLen = minLen
          | otherwise       = goUp (i-1) curLen' minLen'
          where
            (curLen', minLen') = updateMinLen i curLen minLen

        goDown i curLen minLen
          | i > w           = minLen
          | curLen > minLen = minLen
          | otherwise       = goDown (i+1) curLen' minLen'
          where
            (curLen', minLen') = updateMinLen i curLen minLen

count :: Eq a => a -> [a] -> Int
count elt = foldl' (\i e -> i + if e == elt then 1 else 0) 0
