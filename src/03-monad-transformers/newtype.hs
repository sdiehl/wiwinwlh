{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Velocity = Velocity { unVelocity :: Double }
  deriving (Eq, Ord)

v :: Velocity
v = Velocity 2.718

x :: Double
x = 2.718

-- Type error is caught at compile time even though
-- they are the same value at runtime!
err = v + x

newtype Quantity v a = Quantity a
  deriving (Eq, Ord, Num, Show)

data Haskeller
type Haskellers = Quantity Haskeller Int

a = Quantity 2 :: Haskellers
b = Quantity 6 :: Haskellers

totalHaskellers :: Haskellers
totalHaskellers = a + b
