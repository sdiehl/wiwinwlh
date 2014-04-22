{-# LANGUAGE Safe #-}

import Unsafe.Coerce
import System.IO.Unsafe

sin :: String
sin = unsafePerformIO $ getLine

mortalsin :: a
mortalsin = unsafeCoerce 3.14 ()
