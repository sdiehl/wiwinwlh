import Pipes
import Pipes.Prelude as P
import System.IO

readF :: FilePath -> Producer String IO ()
readF file = do
    lift $ putStrLn $ "Opened" ++ file
    h <- lift $ openFile file ReadMode
    fromHandle h
    lift $ putStrLn $ "Closed" ++ file
    lift $ hClose h

main :: IO ()
main = runEffect $ readF "foo.txt" >-> P.take 3 >-> stdoutLn
