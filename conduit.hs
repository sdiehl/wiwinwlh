import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO Int
source = CL.sourceList [1..25]

conduit :: Conduit Int IO String
conduit = CL.map show

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

main :: IO ()
main = do
    source $$ conduit =$ sink
