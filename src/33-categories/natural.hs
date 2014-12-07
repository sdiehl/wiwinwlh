{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

type (⇒) f g = forall a. f a -> g a

headMay :: [] ⇒ Maybe
headMay []    = Nothing
headMay (x:_) = Just x
