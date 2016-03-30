{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class MyClass a b where
  fn :: (a,b)

instance {-# INCOHERENT #-} MyClass a Int where
  fn = error "general"

instance {-# INCOHERENT #-} MyClass Int Int where
  fn = error "specific"

example :: (Int, Int)
example = fn
