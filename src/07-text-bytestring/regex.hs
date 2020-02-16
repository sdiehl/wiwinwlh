{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Text.Regex.TDFA

-- | Verify url address
url :: Text -> Bool
url input = input =~ urlRegex
  where
    urlRegex :: Text
    urlRegex = "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)"

-- | Verify email address
email :: Text -> Bool
email input = input =~ emailRegex
  where
    emailRegex :: Text
    emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"
