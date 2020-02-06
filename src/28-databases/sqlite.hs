{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Text as T
import Database.SQLite.Simple as SQL

selectBooks :: SQL.Connection -> IO [(Int, T.Text, Int)]
selectBooks conn = SQL.query_ conn "select id, title, author_id from books"

main :: IO ()
main = do
  conn <- open "books.db"
  books <- selectBooks conn
  pure ()
