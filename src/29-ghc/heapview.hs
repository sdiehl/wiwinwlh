{-# LANGUAGE MagicHash #-}

import GHC.Exts
import GHC.HeapView

import System.Mem

main :: IO ()
main = do
  -- Constr
  clo <- getClosureData $! ([1,2,3] :: [Int])
  print clo

  -- Thunk
  let thunk = id (1+1)
  clo <- getClosureData thunk
  print clo

  -- evaluate to WHNF
  thunk `seq` return ()

  -- Indirection
  clo <- getClosureData thunk
  print clo

  -- force garbage collection
  performGC

  -- Value
  clo <- getClosureData thunk
  print clo
