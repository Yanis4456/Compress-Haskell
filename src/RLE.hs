{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE(compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = compress' xs x 1
  where
    compress' :: Eq a => [a] -> a -> Int -> [(a, Int)]
    compress' [] currentRun runLength = [(currentRun, runLength)]
    compress' (y:ys) currentRun runLength
      | y == currentRun = compress' ys currentRun (runLength + 1)
      | otherwise       = (currentRun, runLength) : compress' ys y 1

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just []
uncompress xs = sequence $ concatMap (\(x, n) -> replicate n (Just x)) xs
