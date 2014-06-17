import Pipes
import Pipes.Safe
import qualified Pipes.Prelude as P

import System.Timeout (timeout)
import Data.ByteString.Char8
import qualified System.ZMQ as ZMQ

data Opts = Opts
  { _addr    :: String  -- ^ ZMQ socket address
  , _timeout :: Int     -- ^ Time in milliseconds for socket timeout
  }

recvTimeout :: Opts -> ZMQ.Socket a -> Producer ByteString (SafeT IO) ()
recvTimeout opts sock = do
  body <- liftIO $ timeout (_timeout opts) (ZMQ.receive sock [])
  case body of
    Just msg -> do
      liftIO $ ZMQ.send sock msg []
      yield msg
      recvTimeout opts sock
    Nothing  -> liftIO $ print "socket timed out"

collect :: ZMQ.Context
        -> Opts
        -> Producer ByteString (SafeT IO) ()
collect ctx opts = bracket zinit zclose (recvTimeout opts)
  where
    -- Initialize the socket
    zinit = do
      liftIO $ print "waiting for messages"
      sock <- ZMQ.socket ctx ZMQ.Rep
      ZMQ.bind sock (_addr opts)
      return sock

    -- On timeout or completion guarantee the socket get closed.
    zclose sock = do
      liftIO $ print "finalizing"
      ZMQ.close sock

runZmq :: ZMQ.Context -> Opts -> IO ()
runZmq ctx opts = runSafeT $ runEffect $
  collect ctx opts >-> P.take 10 >-> P.print

main :: IO ()
main = do
  ctx <- ZMQ.init 1
  let opts = Opts {_addr = "tcp://127.0.0.1:8000", _timeout = 1000000 }
  runZmq ctx opts
  ZMQ.term ctx
