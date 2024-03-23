{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : ???
-}
module LZ.LZW(compress, uncompress) where

-- | LZW compress method
compress :: String -> [Int]
compress _ = undefined -- TODO

-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress _ = undefined -- TODO
