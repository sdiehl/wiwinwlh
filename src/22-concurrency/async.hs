import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time

timeit :: IO a -> IO (a,Double)
timeit io = do
  t0 <- getCurrentTime
  a <- io
  t1 <- getCurrentTime
  return (a, realToFrac (t1 `diffUTCTime` t0))

worker :: Int -> IO Int
worker n = do
  -- simulate some work
  threadDelay (10^2 * n)
  return (n * n)

-- Spawn 2 threads in parallel, halt on both finished.
test1 :: IO (Int, Int)
test1 = do
  val1 <- async $ worker 1000
  val2 <- async $ worker 2000
  (,) <$> wait val1 <*> wait val2

-- Spawn 2 threads in parallel, halt on first finished.
test2 :: IO (Either Int Int)
test2 = do
  let val1 = worker 1000
  let val2 = worker 2000
  race val1 val2

-- Spawn 10000 threads in parallel, halt on all finished.
test3 :: IO [Int]
test3 = mapConcurrently worker [0..10000]

main :: IO ()
main = do
  print =<< timeit test1
  print =<< timeit test2
  print =<< timeit test3
