{-# LANGUAGE TypeApplications #-}

import Data.Proxy

a :: Proxy Int
a = Proxy @Int

b :: String
b = show (read @Int "42")
