{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

data Book = Book
  { id_ :: Int
  , title :: T.Text
  , author_id :: Int
  } deriving (Show)

instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field

creds :: SQL.ConnectInfo
creds = SQL.defaultConnectInfo
  { SQL.connectUser = "example"
  , SQL.connectPassword = "example"
  , SQL.connectDatabase = "booktown"
  }

selectBooks :: SQL.Connection -> IO [Book]
selectBooks conn = SQL.query_ conn "select id, title, author_id from books limit 4"

main :: IO ()
main = do
  conn <- SQL.connect creds
  books <- selectBooks conn
  print books
