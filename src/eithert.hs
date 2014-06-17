import Control.Error
import Control.Monad.Trans

data Failure
  = NonPositive Int
  | ReadError String
  deriving Show

main :: IO ()
main = do
  putStrLn "Enter a positive number."
  s <- getLine

  e <- runEitherT $ do
      n <- tryRead (ReadError s) s
      if n > 0
        then return $ n + 1
        else throwT $ NonPositive n

  case e of
      Left  n -> putStrLn $ "Failed with: " ++ show n
      Right s -> putStrLn $ "Succeeded with: " ++ show s
