{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State
import System.Random

data Vector = Vector
    { _x :: Double
    , _y :: Double
    } deriving (Show)

data Box = Box
    { _particles :: [Particle]
    } deriving (Show)

data Particle = Particle
    { _pos :: Vector
    , _vel :: Vector
    } deriving (Show)

makeLenses ''Box
makeLenses ''Particle
makeLenses ''Vector

step :: StateT Box IO ()
step = zoom (particles.traverse) $ do
    dx <- use (vel.x)
    dy <- use (vel.y)
    pos.x += dx
    pos.y += dy

particle :: IO Particle
particle = do
  vx <- randomIO
  vy <- randomIO
  return $ Particle (Vector 0 0) (Vector vx vy)

simulate :: IO Box
simulate = do
  particles <- replicateM 5 particle
  let simulation = replicateM 5 step
  let box = Box particles
  execStateT simulation box

main :: IO ()
main = simulate >>= print
