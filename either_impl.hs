instance Monad (Either e) where
  return x = Right x

  (Left x)  >>= f = Left x
  (Right x) >>= f = f x
