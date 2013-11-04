module Main
       where

import Common (inputLinesInteract)
import Control.DeepSeq
import Data.Array
import Data.List.Split (wordsBy)

data State = State { curLine  :: !Int
                   , curState :: !(Array Int Int)
                   } deriving (Eq, Show)

initialState :: State
initialState = State { curLine  = -1
                     , curState = listArray (1,0) []
                     }

main :: IO ()
main = print . extractAnswer =<< inputLinesInteract processLine initialState
  where
    extractAnswer :: State -> Int
    extractAnswer state = let s = curState state
                          in  s ! (snd . bounds $ s)

    processLine :: State -> String -> State
    processLine s l =
      let input     = map read . wordsBy (== ',') $ l
          inplen    = length input
          inparr    = listArray (0, inplen - 1) input
          curLine'  = curLine s + 1
          curState' = listArray (0, inplen - 1)
                      [ updateCell (curLine',i) inparr (curState s) curState'
                      | i <- [0..(inplen - 1)]]
      in State curLine' $!! curState'

    updateCell :: (Int, Int)
               -> Array Int Int -> Array Int Int -> Array Int Int
               -> Int
    updateCell (0,0) inpArr _oldState _thisState =
      inpArr ! 0
    updateCell (0,x) inpArr _oldState  thisState =
      thisState ! (x-1) + inpArr ! x
    updateCell (_,0) inpArr  oldState _thisState =
      oldState ! 0  + inpArr ! 0
    updateCell (_,x) inpArr  oldState  thisState =
      min (oldState ! x) (thisState ! (x-1)) + inpArr ! x
