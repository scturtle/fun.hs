{-# LANGUAGE ForeignFunctionInterface #-}

{-
 Sieve (FFI):
 $ cabal sandbox init
 $ cabal install vector
 $ gcc -O2 -c sieve.c -o libsieve.o
 $ ghc-sandbox libsieve.o sieve.hs -O2 -o sieve
 $ time ./sieve
-}

import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

foreign import ccall unsafe "sieve"
  c_sieve :: CInt -> Ptr a -> IO ()

sieve :: Int -> V.Vector CUChar
sieve n = unsafePerformIO $ do
  v <- VM.unsafeNew n
  let (vptr, _) = VM.unsafeToForeignPtr0 v
  withForeignPtr vptr $ \ptr ->
    c_sieve (fromIntegral n) ptr
  V.unsafeFreeze v

main :: IO ()
main = do
  let isprime = sieve 100000000
  print . V.length $ V.filter (>0) isprime
