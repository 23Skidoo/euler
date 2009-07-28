{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
    where

import Common (inputFileInteract)
import Data.Graph.Inductive
import Data.Maybe

-- OK, my solution is looong and unelegant, but I've learned some
-- things about FGL and graphs by the way. Solution is also quite fast.

-- Here is how it looks when done by the pros:
-- count :: (Ord a, Num a) => [[a]] -> [a]
-- count []       = []
-- count [xs]     = xs
-- count (xs:xss) = let cs = count xss
--                  in zipWith (+) xs (zipWith max (init cs) (tail cs))

type TriangleGraph = Gr () Int
type AnnotatedTriangleGraph = Gr Int Int

-- Test data
triangle :: [[Int]]
triangle = [[3],
            [7, 5],
            [2, 4, 6],
            [8, 5, 9, 3]]

trg :: TriangleGraph
trg = toGraph triangle

-- Convert string lists of form
-- 1
-- 2 3
-- 4 5 6
-- to int lists of form [[1],[2,3],[4,5,6]]
toIntList :: [String] -> [[Int]]
toIntList [] = []
toIntList (x:xs) = (map read $ words x) : toIntList xs

-- Convert int lists to a triangle graph
toGraph :: [[Int]] -> TriangleGraph
toGraph l = mkGraph (nodesList l) (edgesList l)

nodesList :: [[a]] -> [UNode]
nodesList l = (-1,()) : [(x, ()) | x <- [0..sum $ map length l]]

edgesList :: [[Int]] -> [LEdge Int]
edgesList []        = error "Too few arguments"
edgesList [_]       = edgesList []
edgesList (x:x1:xs) = edgesList1 x ++ edgesList2 x1
                      ++ edgesListRest ++ edgesListLast
    where
      edgesList1 :: [Int] -> [LEdge Int]
      edgesList1 [a] = [(0,1,a)]
      edgesList1 _   = error "edgesList1: incorrect argument!"

      edgesList2 :: [Int] -> [LEdge Int]
      edgesList2 [a,b] = [(1,2,a), (1,3,b)]
      edgesList2 _   = error "edgesList2: incorrect argument!"

      edgesListRest :: [LEdge Int]
      edgesListRest = foldr1 (++) [f a | f <- take (length xs) fns | a <- xs]

      fns :: [[Int] -> [LEdge Int]]
      fns = [ edgesListN c l True | c <- firsts | l <- [3,4..]]

      -- 4,7,16,22...
      firsts :: [Int]
      firsts = map (\n -> n*(n+1) `div` 2 + 1) [2..]

      edgesListN :: Int -> Int -> Bool -> [Int] -> [LEdge Int]
      edgesListN _ _ _ []        = []
      edgesListN c l False [a]   = [(c-l, c, a)]
      edgesListN c l True (a:aa) = (c-l+1, c, a):(edgesListN (c+1) l False aa)
      edgesListN c l _ (a:aa)
          = (c-l, c, a):((c-l+1, c, a):(edgesListN (c+1) l False aa))

      -- Link all edges in the base of the triangle to the -1 node
      edgesListLast = [(a,-1,0) | a <- [c..c+l-1]]
          where
            c = firsts !! ((length xs)-1)
            l = length . last $ xs

-- Convert string list to a graph
readTriangle :: [String] -> TriangleGraph
readTriangle = toGraph . toIntList

findMax :: Ord b => (a -> b) -> [a] -> b
findMax extractFun = (foldr1 max) . (map extractFun)

findMaxEdgeLab :: Ord a => [LEdge a] -> a
findMaxEdgeLab = findMax (\(_, _, b) -> b)

findMaxNodeLab :: Ord a => [LNode a] -> a
findMaxNodeLab = findMax (\(_, b) -> b)

changeNodeLabel :: DynGraph gr => a -> Node -> gr a b -> gr a b
changeNodeLabel newLabel node gr = g'
    where
      (mctx, g) = match node gr
      newctx = case mctx of
                 Just (adja, n, _, adjb) -> (adja, n, newLabel, adjb)
                 Nothing -> error "Node not found!"
      g' = newctx & g

checkNotNull :: (Num t) => ([a] -> t) -> [a] -> t
checkNotNull f l = if null l then 0 else f l

nodeLabels :: Graph gr => [Node] -> gr a b -> [a]
nodeLabels n g = map fromJust $ filter isJust $ map (lab g) $ n

annotateNodes :: TriangleGraph -> AnnotatedTriangleGraph
annotateNodes gr = annotateNodes' (nmap (\_ -> 0) gr) (topsort gr)
    where
      annotateNodes' g [] = g
      annotateNodes' g (x:xs) = let g' = annotateNode x g
                                in annotateNodes' g' xs

      annotateNode x g = let innodelabs = nodeLabels (pre g x) g
                             inedges = inn g x
                             maxinedgelab = checkNotNull findMaxEdgeLab inedges
                             maxinnodelab = checkNotNull (findMax id) innodelabs
                         in changeNodeLabel (maxinnodelab + maxinedgelab) x g

solve :: TriangleGraph -> Int
solve gr = (findMax id) $ nodeLabels (pre gr' (-1)) gr'
    where
      gr' = annotateNodes gr

main :: IO ()
main = inputFileInteract (solve . readTriangle . lines)
