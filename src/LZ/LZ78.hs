{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : ???
-}
module LZ.LZ78(compress, uncompress) where

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress _ = undefined -- TODO

-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(Int, Char)] -> Maybe String
uncompress _ = undefined -- TODO
