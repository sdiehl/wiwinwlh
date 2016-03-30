{-# LANGUAGE QuasiQuotes #-}

import Multiline (s)
import qualified Data.Text as T

foo :: T.Text
foo = [s|
This
is
my
multiline
string
|]
