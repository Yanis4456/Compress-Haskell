import Data.Map (Map)
import Data.List
import qualified Data.Map as Map


occurrences :: Ord a => [a] -> Map a Int
occurrences a = makeList a Map.empty

makeList :: Ord a => [a] -> Map a Int -> Map a Int
makeList [] acc = acc
makeList l acc = makeList (tail l) (calculate (head l) acc) 

calculate :: Ord a => a -> Map a Int -> Map a Int
calculate x m = Map.insertWith (+) x 1 m

intToDouble :: Int -> Double
intToDouble i = fromIntegral i

entropy :: Ord a => [a] -> Double
entropy a = 
    let n = intToDouble (length a)
        occu = Map.map intToDouble (occurrences a)
        my_sum :: Double -> Double -> Double
        my_sum ns somme = somme + (ns / n) * logBase 2 (ns / n)      
    in - (Map.foldr my_sum 0.0 occu)


orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts a =
 let list = Map.toList (occurrences a)
 in Data.List.sortOn (\(x, y) -> y) list

main:: IO()
main = print (orderedCounts [1, 2, 3, 4, 1, 2, 2, 3, 1]) 
