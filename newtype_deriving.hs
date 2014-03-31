{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader
import Control.Monad.State

type Output = [Int]
type Input = Int

newtype Comp a = Comp { unComp :: ReaderT Input (State Output) a }
  deriving (Monad, MonadReader Input, MonadState Output)

execComp :: Comp a -> Input -> Output
execComp m input = execState (runReaderT (unComp m) input) []

add :: Comp ()
add = modify $ \s -> [sum s]

scale :: Int -> Comp ()
scale k = modify $ map (*k)

append :: Int -> Comp ()
append x = modify $ \s -> (x : s)

computation :: Comp Int
computation = do
  n <- ask
  append 1
  append 2
  add
  scale 4
  append n
  add
  return 1

example1 :: Output
example1 = execComp computation 0
