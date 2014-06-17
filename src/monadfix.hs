{-# LANGUAGE RecursiveDo #-}

import Control.Applicative
import Control.Monad.Fix

stream1 :: Maybe [Int]
stream1 = do
  rec xs <- Just (1:xs)
  return (map negate xs)

stream2 :: Maybe [Int]
stream2 = mfix $ \xs -> do
  xs' <- Just (1:xs)
  return (map negate xs')
