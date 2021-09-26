{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Aeson (object, (.=), Value (Null))
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types
import System.Directory
import Network.Wai.Middleware.HttpAuth
import Network.HTTP.Types.Status (noContent204)

import Auth
import Model

main :: IO ()
main = do
  doMigration
  scotty 3000 $ do
    get  "/api/post"      $ getPostsA
    get  "/api/post/:pid" $ param "pid" >>= getPostByIdA
    --middleware $ basicAuth (verify) authSettings
    --Sc.post "/api/content"      $ postContent conn

getPostsA :: ActionM ()
getPostsA = do
  posts <- liftIO getPosts
  json $ object ["posts" .= posts]

getPostByIdA :: Integer -> ActionM ()
getPostByIdA index = do
  post <- liftIO $ getPostById index
  maybe noContentA json post

noContentA :: ActionM ()
noContentA = do
  status noContent204
  json Null
