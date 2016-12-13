{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Foundation
import Foundation.IO
import Foundation.String
import Foundation.VFS.FilePath

import Foundation.Collection

example :: String
example = "Violence is the last refuge of the incompetent."

bytes :: UArray Word8
bytes = toBytes UTF8 example

file :: IO (UArray Word8)
file = readFile "foundation.hs"

fileString :: IO (String, Maybe ValidationFailure, UArray Word8)
fileString = fromBytes UTF8 <$> file


xs :: NonEmpty [Int]
xs = fromList [1,2,3]

x :: Int
x = head xs
