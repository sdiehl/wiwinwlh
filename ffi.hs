-- ghc qsort.o ffi.hs -o ffi

{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

foreign import ccall safe "sort" qsort
    :: Ptr a -> CInt -> CInt -> IO ()


vecPtr :: VM.Storable a => VM.MVector s a -> Ptr a
vecPtr = unsafeForeignPtrToPtr . fst . VM.unsafeToForeignPtr0

main :: IO ()
main = do
  let vs  = V.fromList ([1,3,5,2,1,2,5,9,6] :: [CInt])
  v <- V.thaw vs
  qsort (vecPtr v) 0 9
  out <- V.freeze v
  print out
