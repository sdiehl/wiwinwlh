{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Trace

data Example
  = Example
      { x :: Int,
        y :: Int
      }
  deriving (Show)

data MyFail = MyFail
  deriving (Show)

example1 :: Member (State Example) r => Sem r ()
example1 = do
  modify $ \s -> s {x = 1}
  modify $ \s -> s {y = 2}
  pure ()

example2 :: Members '[Trace, State Example, Error MyFail] r => Sem r ()
example2 = do
  modify $ \s -> s {x = 1}
  modify $ \s -> s {y = 2}
  --throw MyFail
  trace "foo"
  pure ()

main :: IO ()
main = do
  (s, _) <- runFinal $ embedToFinal @IO $ runState (Example 0 0) example1
  res <-
    runFinal
      $ embedToFinal @IO
      $ errorToIOFinal @MyFail
      $ runState (Example 0 0)
      $ traceToIO example2
  print s
  print res
  pure ()
