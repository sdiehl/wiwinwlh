{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class MyClass a b where
  fn :: (a,b)

instance MyClass Int b where
  fn = error "a"

instance MyClass a Int where
  fn = error "b"

example :: (Int, Int)
example = fn
