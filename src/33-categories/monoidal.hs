{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

import Prelude hiding ((.))

class Category k where
  id :: k a a
  (.) :: k b c -> k a b -> k a c

class Category k => Bifunctor k p where
  bimap :: k a b -> k a' b' -> k (p a a') (p b b')

class Bifunctor k p => Associative k p where
  associate :: k (p (p a b) c) (p a (p b c))
  coassociate :: k (p a (p b c)) (p (p a b) c)

class Associative k p => Monoidal k p i | k p -> i where
  idl :: k (p i a) a
  idr :: k (p a i) a
  coidl :: k a (p i a)
  coidr :: k a (p a i)

class Braided k p where
  braid :: k (p a b) (p b a)

class (Monoidal k prod i, Braided k prod) => Cartesian k prod i | k -> prod i where
  fst :: k (prod a b) a
  snd :: k (prod a b) b
  diag :: k a (prod a a)
  (&&&) :: k a b -> k a c -> k a (prod b c)
  f &&& g = (f `bimap` g) . diag

class Cartesian k p i => CCC k p i e | k -> p i e where
  apply :: k (p (e a b) a) b
  curry :: k (p a b) c -> k a (e b c)
  uncurry :: k a (e b c) -> k (p a b) c
