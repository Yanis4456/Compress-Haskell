{- |
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : ???
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where

import Data.Map (Map)
import Data.List
import qualified Data.Map as Map

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map a Int
occurrences a = makeList a Map.empty    
                                         
makeList :: Ord a => [a] -> Map a Int -> Map a Int
makeList [] acc = acc
makeList l acc = makeList (tail l) (calculate (head l) acc)

calculate :: Ord a => a -> Map a Int -> Map a Int
calculate x m = Map.insertWith (+) x 1 m


-- | SHANNON entropy of source
intToDouble :: Int -> Double
intToDouble i = fromIntegral i                         
 
entropy :: Ord a => [a] -> Double
entropy a =                  
 let n = intToDouble (length a)                                                        
     occu = Map.map intToDouble (occurrences a)
     my_sum :: Double -> Double -> Double                   
     my_sum ns somme = somme + (ns / n) * logBase 2 (ns / n)      
 in - (Map.foldr my_sum 0.0 occu)


-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts a =                                        
  let list = Map.toList (occurrences a)                      
  in Data.List.sortOn (\(x, y) -> y) list  
