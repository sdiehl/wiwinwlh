{-# LANGUAGE MultiWayIf #-}

import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.List as CL

source :: ConduitT () Int IO ()
source = CL.sourceList [1 .. 100]

conduit :: ConduitT Int String IO ()
conduit = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      if  | n `mod` 15 == 0 -> yield "FizzBuzz"
          | n `mod` 5 == 0 -> yield "Fizz"
          | n `mod` 3 == 0 -> yield "Buzz"
          | otherwise -> return ()
      conduit

sink :: ConduitT String o IO ()
sink = CL.mapM_ putStrLn

main :: IO ()
main = runConduit $ source .| conduit .| sink
