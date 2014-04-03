{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B

-- From pack
myBStr1 :: B.ByteString
myBStr1 = B.pack ("foo" :: String)

-- From overloaded string literal.
myBStr2 :: B.ByteString
myBStr2 = "bar"
