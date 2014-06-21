{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

-- From pack
myTStr1 :: T.Text
myTStr1 = T.pack ("foo" :: String)

-- From overloaded string literal.
myTStr2 :: T.Text
myTStr2 = "bar"
