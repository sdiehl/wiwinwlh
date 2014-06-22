{-# LANGUAGE ExplicitForAll #-}

data Iso a b = Iso { to :: a -> b, from :: b -> a }

f :: forall a. Maybe a -> Either () a
f (Just a) = Right a
f Nothing  = Left ()

f' :: forall a. Either () a -> Maybe a
f' (Left _)  = Nothing
f' (Right a) = Just a

iso :: Iso (Maybe a) (Either () a)
iso = Iso f f'

data V = V deriving Eq

ex1 = f  (f' (Right V)) == Right V
ex2 = f' (f  (Just V))  == Just V
