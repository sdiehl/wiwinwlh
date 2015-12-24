import Data.Foldable
import Data.SBV

-- | val [4,2] == 42
val :: [SInteger] -> SInteger
val = foldr1 (\d r -> d + 10*r) . reverse

puzzle :: Symbolic SBool
puzzle = do
  ds@[b,u,r,i,t,o,m,n,a,d] <- sequenceA [ sInteger [v] | v <- "buritomnad" ]
  constrain $ allDifferent ds
  for_ ds $ \d -> constrain $ inRange d (0,9)
  pure $    val [b,u,r,r,i,t,o]
          + val     [m,o,n,a,d]
        .== val [b,a,n,d,a,i,d]



