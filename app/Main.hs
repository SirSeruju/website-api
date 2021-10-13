{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Model
import Actions (getPostsA, getPostByIdA, postPostA)

main :: IO ()
main = do
  doMigration
  scotty 3000 $ do
    get  "/api/post"      $ getPostsA
    get  "/api/post/:pid" $ param "pid" >>= getPostByIdA
    post "/api/post"      $ postPostA
    --middleware $ basicAuth (verify) authSettings
    --Sc.post "/api/content"      $ postContent conn
