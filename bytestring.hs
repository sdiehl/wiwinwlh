{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

-- From pack
myBStr1 :: S.ByteString
myBStr1 = S.pack ("foo" :: String)

-- From overloaded string literal.
myBStr2 :: S.ByteString
myBStr2 = "bar"
