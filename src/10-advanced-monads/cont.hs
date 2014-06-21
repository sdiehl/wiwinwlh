import Control.Monad
import Control.Monad.Cont

add :: Int -> Int -> Cont k Int
add x y = return $ x + y

mult :: Int -> Int -> Cont k Int
mult x y = return $ x * y

contt :: ContT () IO ()
contt = do
    k <- do
      callCC $ \exit -> do
        lift $ putStrLn "Entry"
        exit $ \_ -> do
          putStrLn "Exit"
    lift $ putStrLn "Inside"
    lift $ k ()

callcc :: Cont String Integer
callcc = do
  a <- return 1
  b <- callCC (\k -> k 2)
  return $ a+b

ex1 :: IO ()
ex1 = print $ runCont (f >>= g) id
  where
    f = add 1 2
    g = mult 3
-- 9

ex2 :: IO ()
ex2 = print $ runCont callcc show
-- "3"

ex3 :: IO ()
ex3 = runContT contt print
-- Entry
-- Inside
-- Exit

main :: IO ()
main = do
  ex1
  ex2
  ex3
