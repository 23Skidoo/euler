module Main
    where

import Control.Monad
import Data.Array.IArray
import System

-- Data types

data Direction = GoDown | GoRight | GoDownOrRight | Halt
                 deriving (Eq, Show)

type Grid = (Array (Int, Int) Direction)

data PathTree = WentDown PathTree | WentRight PathTree
              | WentDownAndRight PathTree PathTree | Halted

-- Functions

makeGrid x = let makeNode m n | m == x && n == x  = Halt
                              | m == x            = GoDown
                              | n == x            = GoRight
                              | otherwise         = GoDownOrRight

                 makeArrayNode m n = ((m,n), makeNode m n)

                 grid = array ((0, 0), (x, x)) [makeArrayNode m n
                                                    | m <- [0..x], n <- [0..x]]

             in grid

buildTree :: Grid -> PathTree
buildTree g = let buildTree' :: Grid -> (Int, Int) -> PathTree
                  buildTree' g i@(x,y)
                      | g ! i == Halt
                          = Halted
                      | g ! i == GoDown
                          = WentDown (buildTree' g (x, y+1))
                      | g ! i == GoRight
                          = WentRight (buildTree' g (x+1, y))
                      | g ! i == GoDownOrRight
                          = WentDownAndRight (buildTree' g (x, y+1)) (buildTree' g (x+1, y))

              in buildTree' g (0,0)

countLeaves :: (Num t) => PathTree -> t
countLeaves Halted                    = 1
countLeaves (WentDown t)              = countLeaves t
countLeaves (WentRight t)             = countLeaves t
countLeaves (WentDownAndRight t1 t2)  = countLeaves t1 + countLeaves t2

-- This is too slow for n = 20, so I used the initial output to google the sequence
-- The formula is C(2n, n)
main :: IO ()
main = do args <- getArgs
          case args of
            [] -> computeAnswer 20
            (x:_) -> do let arg = read x
                        computeAnswer arg
    where
      computeAnswer = print . countLeaves . buildTree . makeGrid