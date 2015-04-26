{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M (findWithDefault, fromList)
import qualified Data.ByteString as BS

{-
 - http://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm
 -}

horspool :: BS.ByteString -> BS.ByteString -> Maybe Int
horspool s p = f (lenp - 1) (lenp - 1) (lenp - 1)
  where lenp = BS.length p
        lens = BS.length s
        jmptbl = M.fromList $ zip (BS.unpack p) [lenp - 1, lenp - 2 .. 1]
        jmp c = M.findWithDefault lenp c jmptbl
        f i j k
          | i >= lens = Nothing
          | j == -1 = Just (i + 1)
          | s `BS.index` i == p `BS.index` j = f (i-1) (j-1) k
          | otherwise = let k' = k + jmp (s `BS.index` k)
                         in f k' (lenp - 1) k'

main :: IO ()
main = do
  print $ horspool "aaaabba" "bba"
  print $ horspool "ababbbb" "bbbb"
