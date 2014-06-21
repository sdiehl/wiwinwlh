import Data.DList
import Control.Monad
import Control.Monad.Writer

logger :: Writer (DList Int) ()
logger = replicateM_ 100000 $ tell (singleton 0)
