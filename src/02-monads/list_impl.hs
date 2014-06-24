instance Monad [] where
  m >>= f   =  concat (map f m)
  return x  =  [x]
