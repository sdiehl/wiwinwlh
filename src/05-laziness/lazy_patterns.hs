f :: (a, b) -> Int
f (a,b) = const 1 a

g :: (a, b) -> Int
g ~(a,b) = const 1 a

-- λ: f undefined
-- *** Exception: Prelude.undefined
-- λ: g undefined
-- 1

j :: Maybe t -> t
j ~(Just x) = x

k :: Maybe t -> t
k (Just x) = x

-- λ: j Nothing
-- *** Exception: src/05-laziness/lazy_patterns.hs:15:1-15: Irrefutable pattern failed for pattern (Just x)
--
-- λ: k Nothing
-- *** Exception: src/05-laziness/lazy_patterns.hs:18:1-14: Non-exhaustive patterns in function k
