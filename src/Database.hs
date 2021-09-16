{-# LANGUAGE OverloadedStrings #-}

module Database (create) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

create :: IO ()
create = do
  conn <- open "materials.db"
  execute_ conn $ "CREATE TABLE content (\
                  \id INTEGER PRIMARY KEY,\
                  \user_id INTEGER NOT NULL,\
                  \title TEXT NOT NULL UNIQUE,\
                  \content TEXT NOT NULL,\
                  \FOREIGN KEY (user_id) REFERENCES users(id));"
  execute_ conn "CREATE TABLE users (id INTEGER PRIMARY KEY, user TEXT NOT NULL UNIQUE, password NOT NULL);"
  close conn
