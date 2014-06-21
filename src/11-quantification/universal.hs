{-# LANGUAGE ExplicitForAll #-}

-- ∀a. [a]
example1 :: forall a. [a]
example1 = []

-- ∀a. [a]
example2 :: forall a. [a]
example2 = [undefined]

-- ∀a. ∀b. (a → b) → [a] → [b]
map' :: forall a. forall b. (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- ∀a. [a] → [a]
reverse' :: forall a. [a] -> [a]
reverse' = foldl (flip (:)) []
