{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash
import Data.ByteArray (convert)
import qualified Data.ByteString as B

segmentSize :: Int
segmentSize = 64

type Hash = Digest SHA256

joinHash :: Hash -> Hash -> Hash
joinHash a b = hash (B.append (convert a) (convert b))

segments :: B.ByteString -> [B.ByteString]
segments bs
  | B.null bs = []
  | otherwise = seg : segments rest where
    (seg, rest) = B.splitAt segmentSize bs

merkleRoot :: [Hash] -> Hash
merkleRoot [h] = h
merkleRoot hs  = joinHash (merkleRoot left) (merkleRoot right)
  where
    (left, right) = splitAt i hs
    i = until (\x -> x*2 >= length hs) (*2) 1

tree :: Hash
tree = merkleRoot $ map hash (["4", "8", "15", "16", "23", "42"] :: [B.ByteString])

file :: IO ()
file = B.readFile "Merkle.hs" >>= print . merkleRoot . map hash . segments
