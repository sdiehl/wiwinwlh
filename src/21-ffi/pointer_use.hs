{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import System.IO
import Foreign.C.Types(CInt(..))

foreign import ccall "wrapper"
  makeFunPtr :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))

foreign import ccall "pointer.c invoke"
  invoke :: FunPtr (CInt -> IO ()) -> IO ()

fn :: CInt -> IO ()
fn n = do
  putStrLn "Hello from Haskell, here's a number passed between runtimes:"
  print n
  hFlush stdout

main :: IO ()
main = do
  fptr <- makeFunPtr fn
  invoke fptr
