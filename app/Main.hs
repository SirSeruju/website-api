{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Aeson (object, (.=))
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types
import System.Directory
import Network.Wai.Middleware.HttpAuth

import Auth
import Model

main :: IO ()
main = do
  doMigration
  scotty 3000 $ do
    get  "/api/post"      $ getPostsA
    --middleware $ basicAuth (verify) authSettings
    --Sc.get  "/api/content/:cid" $ param "cid" >>= (\id -> getContentById id conn)
    --Sc.post "/api/content"      $ postContent conn

getPostsA :: ActionM ()
getPostsA = do
  posts <- liftIO getPosts
  json $ object ["posts" .= posts]
