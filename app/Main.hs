{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty as Sc
import Network.HTTP.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson
import System.Directory

import Content
import Database as Db

databaseName = "materials.db"

main :: IO ()
main = do
  checkDatabase
  conn <- open databaseName
  Sc.scotty 3000 $ do
    Sc.get  "/api/content"      $ getContent conn
    Sc.get  "/api/content/:cid" $ param "cid" >>= (\id -> getContentById id conn)
    Sc.post "/api/content"      $ postContent conn
  close conn

checkDatabase :: IO ()
checkDatabase = do
  doesExist <- doesFileExist databaseName
  case doesExist of
    True -> return ()
    False -> Db.create
