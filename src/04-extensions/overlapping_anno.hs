{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class MyClass a b where
  fn :: (a,b)

instance {-# OVERLAPPING #-} MyClass Int b where
  fn = error "b"

instance {-# OVERLAPPING #-} MyClass a Int where
  fn = error "a"

instance {-# OVERLAPPING #-} MyClass Int Int where
  fn = error "c"

example :: (Int, Int)
example = fn
