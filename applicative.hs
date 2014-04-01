import Network.HTTP
import Data.Functor ((<$>))
import Control.Applicative ((<*>))

fetch1, fetch2 ::  IO String
fetch1 = simpleHTTP (getRequest "http://www.fpcomplete.com/") >>= getResponseBody
fetch2 = simpleHTTP (getRequest "http://www.haskell.org/") >>= getResponseBody

combined ::  IO String
combined = (++) <$> fetch1 <*> fetch2

main :: IO ()
main = combined >>= print
