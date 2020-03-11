instance Monad (Either e) where
  return = Right

  (Left x) >>= f = Left x
  (Right x) >>= f = f x
