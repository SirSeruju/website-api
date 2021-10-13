{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Aeson (object, (.=), Value (Null, Number), fromJSON, toJSON, Result(..))
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types
import System.Directory
import Network.Wai.Middleware.HttpAuth
import Network.Wai
import Network.HTTP.Types.Status (noContent204)
import Data.ByteString
import GHC.Int
import Data.HashMap.Strict
import Data.Text
import Control.Exception

import Auth
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
