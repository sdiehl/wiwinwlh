import Control.Monad.State
import Control.Monad.Morph

type Eval a = State [Int] a

runEval :: [Int] -> Eval a -> a
runEval = flip evalState

pop :: Eval Int
pop = do
  top <- gets head
  modify tail
  return top

push :: Int -> Eval ()
push x = modify (x:)

ev1 :: Eval Int
ev1 = do
  push 3
  push 4
  pop
  pop

ev2  :: StateT [Int] IO ()
ev2 = do
  result <- hoist generalize ev1
  liftIO $ putStrLn $ "Result: " ++ show result
