{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Selda
import Database.Selda.SQLite

data Employee
  = Employee
      { id :: ID Employee,
        name :: Text,
        title :: Text,
        companyId :: ID Company
      }
  deriving (Generic)

data Company
  = Company
      { id :: ID Company,
        name :: Text
      }
  deriving (Generic)

instance SqlRow Employee

instance SqlRow Company

-- Generate tables for Haskell datatypes
employees :: Table Employee
employees = table "employees" [#id :- autoPrimary, #companyId :- foreignKey companies #id]

companies :: Table Company
companies = table "companies" [#id :- autoPrimary]

exampleSelect :: IO ([Employee], [Company])
exampleSelect = withSQLite "company.sqlite" $ do
  xs <- query (select employees)
  ys <- query (select companies)
  pure (xs, ys)

main :: IO ()
main = withSQLite "company.sqlite" $ do
  createTable employees
  createTable companies
  -- Populate companies
  insert_
    companies
    [Company (toId 0) "Dunder Mifflin Inc."]
  -- Populate employees
  insert_
    employees
    [ Employee (toId 0) "Michael Scott" "Director" (toId 0),
      Employee (toId 1) "Dwight Schrute" "Regional Manager" (toId 0)
    ]
