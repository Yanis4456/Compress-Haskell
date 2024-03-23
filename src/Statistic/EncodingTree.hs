{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where

import Statistic.Bit

data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a)
                    | EncodingLeaf Int a
  deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True
isLeaf  _                 = False

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _  ) = cnt
count (EncodingNode cnt _ _) = cnt

-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
(EncodingLeaf _ sbl) `has` element = element == sbl
(EncodingNode _ left right) `has` element = (has left element) || (has right element)

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode tree a                 
 |has tree a == False = Nothing
 |otherwise = chemin tree a []
 
chemin :: Eq a => EncodingTree a -> a -> [Bit] -> Maybe [Bit]
chemin (EncodingNode _ left right) element acc                                        
 |has left element == True = (chemin left element (acc ++ [Zero]))
 |otherwise = (chemin right element (acc ++ [One]))           
 
chemin (EncodingLeaf _ val) element acc    
 |val == element = Just acc 
 |otherwise = Nothing

-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ val) bits = Just (val, bits)
decodeOnce (EncodingNode _ left right) bits
 |null bits == True = Nothing
 |head bits == Zero = decodeOnce left (tail bits)
 |head bits == One = decodeOnce right (tail bits)
 |otherwise = Nothing

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits = decodeH tree bits []

decodeH :: EncodingTree a -> [Bit] -> [a] -> Maybe [a]
decodeH tree bits acc =
    let result = decodeOnce tree bits
    in 
      case result of  
        Just (val, bits_restant) ->          
          if not (null bits_restant)      
            then decodeH tree bits_restant (acc ++ [val])        
            else Just (acc ++ [val])        
        _ -> Nothing 

-- | Mean length of the binary encoding
intToDouble :: Int -> Double
intToDouble i = fromIntegral i 

meanLength :: EncodingTree a -> Double
meanLength tree =  (meanLength' tree 0 0) / (intToDouble (count tree))   

meanLength' :: EncodingTree a -> Int -> Double -> Double
meanLength' (EncodingLeaf et _) acc _ = intToDouble et * intToDouble acc 
meanLength' (EncodingNode _ left right) acc somme = somme + meanLength' left (acc + 1) 0 + meanLength' right (acc + 1) 0 

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
join :: Maybe [Bit] -> [Bit]
join Nothing = []
join (Just bits) = bits

compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])  
compress fct list =   
 let tree = fct list 
 in (tree, compress' tree list [])
 
compress' :: Eq a => Maybe (EncodingTree a) -> [a] -> [Bit] -> [Bit]
compress' Nothing _ _ = []
compress' _ [] acc = acc
compress' tree list acc = compress' tree (tail list) (acc ++ compress'' tree (head list))

compress'' :: Eq a => Maybe (EncodingTree a) -> a -> [Bit]
compress'' Nothing _ = []
compress'' (Just tree) element = join (encode tree element) 

-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Nothing, _) = Nothing
uncompress ((Just tree), bits) = decode tree bits

