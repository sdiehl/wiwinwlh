{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits

instance
  -- Error Message
  TypeError (Text "Equality is not defined for functions"
  :$$:
  (ShowType a :<>: Text " -> " :<>: ShowType b))

  -- Instance head
  => Eq (a -> b) where (==) = undefined

-- Fail when we try to equate two functions
example = id == id
