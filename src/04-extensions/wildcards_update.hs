{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text

data  Example = Example
  { e1 :: Int
  , e2 :: Text
  , e3 :: Text
  } deriving (Show)

-- Extracting from a record using wildcards.
scope :: Example -> (Int, Text, Text)
scope Example {..} = (e1, e2, e3)

-- Assign to a record using wildcards.
assign :: Example
assign = Example {..}
  where
    (e1, e2, e3) = (1, "Kirk", "Picard")
