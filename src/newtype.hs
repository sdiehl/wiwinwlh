{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Velocity = Velocity { unVelocity :: Double }
  deriving (Eq, Ord)

v :: Velocity
v = Velocity 2.718

x :: Double
x = 6.636

err = v + x
