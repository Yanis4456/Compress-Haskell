import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)
data Bit = Zero | One
  deriving (Bounded, Enum, Eq, Ord)

instance Show Bit where
  show Zero = "0"
  show One  = "1"

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
(EncodingLeaf _ sbl) `has` elem = elem == sbl
(EncodingNode _ left right) `has` elem = (has left elem) || (has right elem)

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode tree a 
 |has tree a == False = Nothing
 |otherwise = chemin tree a []

chemin (EncodingNode et left right) elem acc 
 |has left elem == True = (chemin left elem (acc ++ [Zero])) 
 |otherwise = (chemin right elem (acc ++ [One]))

chemin (EncodingLeaf et val) elem acc 
 |val == elem = Just acc 
 |otherwise = Nothing


decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ val) bits = Just (val, bits)
decodeOnce (EncodingNode _ left right) bits 
 |null bits == True = Nothing
 |head bits == Zero = decodeOnce left (tail bits)
 |head bits == One = decodeOnce right (tail bits)

decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits = decodeH tree bits [] 


decodeH tree bits acc =
    let result = decodeOnce tree bits
    in
      case result of
        Just (val, bits_restant) ->
          if not (null bits_restant)
            then decodeH tree bits_restant (acc ++ [val])
            else Just (acc ++ [val])
        _ -> Nothing

intToDouble :: Int -> Double
intToDouble i = fromIntegral i                         
 

meanLength :: EncodingTree a -> Double
meanLength tree =  (meanLength' tree 0 0) / (intToDouble (count tree))

meanLength' :: EncodingTree a -> Int -> Double -> Double
meanLength' (EncodingLeaf et val) acc _ = intToDouble et * intToDouble acc 
meanLength' (EncodingNode et left right) acc somme = somme + meanLength' left (acc + 1) 0 + meanLength' right (acc + 1) 0 

join :: Maybe [Bit] -> [Bit]
join Nothing = []
join (Just bits) = bits

compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress fct list =
 let tree = fct list 
 in (tree, compress' tree list [])

compress' Nothing _ _ = []
compress' tree [] acc = acc
compress' tree list acc = compress' tree (tail list) (acc ++ compress'' tree (head list))

compress'' Nothing _ = []
compress'' (Just tree) elem = join (encode tree elem) 

uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Nothing, _) = Nothing
uncompress ((Just tree), bits) = decode tree bits


-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map a Int
occurrences a = makeList a Map.empty    
                                         
makeList :: Ord a => [a] -> Map a Int -> Map a Int
makeList [] acc = acc
makeList l acc = makeList (tail l) (calculate (head l) acc)

calculate :: Ord a => a -> Map a Int -> Map a Int
calculate x m = Map.insertWith (+) x 1 m


-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts a =                                        
  let list = Map.toList (occurrences a)                      
  in Data.List.sortOn (\(x, y) -> y) list  

tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree my_list = 
 let ordered = map (\(a, b) -> ([a], b)) (Data.List.reverse (orderedCounts my_list))
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

fuse :: Ord a => ([a], Int) -> ([a], Int) -> ([a], Int)
fuse (s1, eff1) (s2, eff2) = (s1 ++ s2, eff1 + eff2) 
-------------------------
tree2 :: Ord a => [a] -> Maybe (EncodingTree a)
tree2 [] = Nothing
tree2 my_list = Just (makeTree (fst start_tree) (snd start_tree) s1 s2)
 where ordered = Data.List.reverse (orderedCounts my_list)
      
       split n1 n2 _ [(_,_), (_,_)] = (0, n1, n2)
       split n1 n2 i list = let value = snd (list !! i)
          in
            if n1 + value >= n2 - value
              then if n2 - n1 > value
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



main :: IO()
main = 
 --let tree = (EncodingNode 6 (EncodingNode 5 (EncodingLeaf 3 "a") (EncodingLeaf 2 "d")) (EncodingLeaf 1 "c"))
  --let order = Data.List.reverse (orderedCounts ["a", "b", "b", "a", "c"])
  --in print (foldl fuse ([], 0) order)
 print (compress tree2 "abbca")




















