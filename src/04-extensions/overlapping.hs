{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class MyClass a b where
  fn :: (a,b)

instance MyClass Int b where
  fn = error "b"

instance MyClass a Int where
  fn = error "a"

instance MyClass Int Int where
  fn = error "c"

example :: (Int, Int)
example = fn
