{-# LANGUAGE MultiWayIf #-}

import Data.Conduit
import Control.Monad.Trans
import qualified Data.Conduit.List as CL

source :: Source IO Int
source = CL.sourceList [1..100]

conduit :: Conduit Int IO String
conduit = do
  val <- await
  liftIO $ print val
  case val of
    Nothing -> return ()
    Just n -> do
      if | n `mod` 15 == 0 -> yield "FizzBuzz"
         | n `mod` 5  == 0 -> yield "Fizz"
         | n `mod` 3  == 0 -> yield "Buzz"
         | otherwise       -> return ()
      conduit

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

main :: IO ()
main = source $$ conduit =$ sink
