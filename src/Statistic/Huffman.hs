{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}
module Statistic.Huffman(tree) where

import Statistic.EncodingTree
import Statistic.Source
import Data.List()

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree my_list = 
 let ordered = map (\(a, b) -> ([a], b)) (reverse (orderedCounts my_list))
     fusions = scanr fuse ([], 0) ordered
 in makeTree fusions                    

makeTree :: Ord a => [([a], Int)] -> Maybe (EncodingTree a)
makeTree ((x1, y1): (x2, y2): t) =
 if length t > 0
  then let left = makeTree ([(x2, y2)] ++ t)
       in
        case left of
         Just (leftTree) -> Just (EncodingNode y1 (EncodingLeaf (y1 - y2) (head x1)) leftTree)
         _ -> Nothing
  else Just (EncodingLeaf y1 (head x1))
makeTree _ = Nothing

fuse :: ([a], Int) -> ([a], Int) -> ([a], Int)
fuse (s1, eff1) (s2, eff2) = (s1 ++ s2, eff1 + eff2)
