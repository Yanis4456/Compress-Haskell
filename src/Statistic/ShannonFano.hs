{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
module Statistic.ShannonFano(tree) where

import Statistic.EncodingTree
import Statistic.Source
import Data.List()

-- | Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing
tree my_list = Just (makeTree (fst start_tree) (snd start_tree) s1 s2)
 where ordered = reverse (orderedCounts my_list)
      
       split n1 n2 _ [(_,_), (_,_)] = (0, n1, n2)
       split n1 n2 i list = let value = snd (list !! i)
          in
            if n1 + value >= n2 - value
              then if n2 - n1 >= value
                    then  (i, (n1 + value), (n2 - value))
                    else (i - 1, n1, n2)
               else split (n1 + value) (n2 - value) (i + 1) list
       
       (start, s1, s2) = split 0 (length my_list) 0 ordered
       start_tree = splitAt (start+1) ordered

       makeTree [] [(val, et)] _ _ = EncodingLeaf et val
       makeTree [(val, et)] [] _ _ = EncodingLeaf et val

       makeTree [(val, et)] left len1 len2 =
         let (left_index, l_n1, l_n2) = split 0 len1 0 left
             left_tree = splitAt (left_index+1) left
         in EncodingNode (len1 + len2) (EncodingLeaf et val) (makeTree (fst left_tree) (snd left_tree) l_n1 l_n2)
      
       makeTree left [(val, et)] len1 len2 =
         let (left_index, l_n1, l_n2) = split 0 len1 0 left             
             left_tree = splitAt (left_index+1) left
         in EncodingNode (len1 + len2) (makeTree (fst left_tree) (snd left_tree) l_n1 l_n2) (EncodingLeaf et val)

       makeTree left right len1 len2 =
         let (left_index, l_n1, l_n2) = split 0 len1 0 left
             (right_index, r_n1, r_n2) = split 0 len2 0 right
             left_tree =  splitAt (left_index+1) left
             right_tree = splitAt (right_index+1) right
         in EncodingNode (len1 + len2) (makeTree (fst left_tree) (snd left_tree) l_n1 l_n2) (makeTree (fst right_tree) (snd right_tree) r_n1 r_n2)
