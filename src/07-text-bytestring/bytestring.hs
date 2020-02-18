{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

-- From pack
bstr1 :: S.ByteString
bstr1 = S.pack [102, 111, 111] -- ascii encoding of foo as [Word8]

-- From overloaded string literal.
bstr2 :: S.ByteString
bstr2 = "bar"
