{-# LANGUAGE OverloadedStrings #-}

module Database (create) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

create :: IO ()
create = do
  conn <- open "materials.db"
  execute_ conn "CREATE TABLE content (id INTEGER PRIMARY KEY, name text NOT NULL UNIQUE, html text NOT NULL);"
  execute_ conn "CREATE TABLE users (id INTEGER PRIMARY KEY, user text NOT NULL UNIQUE, password NOT NULL);"
  close conn
