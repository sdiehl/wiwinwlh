import System.Random

data Cat = Cat String deriving Show

data State a = Live a | Dead a deriving Show

decayed :: StdGen -> Bool
decayed gen = fst $ random gen

box :: StdGen -> Cat -> State Cat
box gen x = if decayed gen then Live x else Dead x

main :: IO ()
main = do
  gen <- getStdGen
  let cat = Cat "Fluffy"
  print $ box gen cat
