{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

-- a b are existentially bound type variables, m is a free type variable
data MonadI m = MonadI
  { _return :: forall a . a -> m a
  , _bind   :: forall a b . m a -> (a -> m b) -> m b
  }

monadMaybe:: MonadI Maybe
monadMaybe = MonadI
  { _return = Just
  , _bind   = \m f -> case m of
      Nothing -> Nothing
      Just x  -> f x
  }
