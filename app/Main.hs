{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty as Sc
import Network.HTTP.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson

import Content

main :: IO ()
main = do
  conn <- open "materials.db"
  Sc.scotty 3000 $ do
    Sc.get "/api/content" $ getContent conn
    Sc.get "/api/content/:cid" $ param "cid" >>= (\id -> getContentById id conn)
