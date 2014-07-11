-- Double is inferred by type inferencer.
example1 :: Double
example1 = 3.14

-- In the presense of a lambda, a different type is inferred!
example2 :: Fractional a => t -> a
example2 _ = 3.14

default (Integer, Double)
