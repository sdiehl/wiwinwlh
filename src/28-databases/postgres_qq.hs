{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Text as T

import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

data Book = Book
  { id_ :: Int
  , title :: T.Text
  , first_name :: T.Text
  , last_name :: T.Text
  } deriving (Show)

instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field <*> field

creds :: SQL.ConnectInfo
creds = SQL.defaultConnectInfo
  { SQL.connectUser = "example"
  , SQL.connectPassword = "example"
  , SQL.connectDatabase = "booktown"
  }

selectBooks :: SQL.Query
selectBooks = [sql|
select
  books.id,
  books.title,
  authors.first_name,
  authors.last_name
from books
join authors on
  authors.id = books.author_id
limit 5
|]

main :: IO ()
main = do
  conn <- SQL.connect creds
  (books :: [Book]) <- SQL.query_ conn selectBooks
  print books
