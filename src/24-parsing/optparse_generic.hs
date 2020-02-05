{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Options.Generic

data Options = Options
  { verbose :: Bool     <?> "Enable verbose mode"
  , input   :: FilePath <?> "Input file"
  , output  :: FilePath <?> "Output file"
  }
  deriving (Generic, Show, ParseRecord)

main :: IO ()
main = do
  opts <- getRecord "My CLI"
  print (opts :: Options)
