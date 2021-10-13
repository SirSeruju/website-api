{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
  ( scottyOpts
  , Options (Options, verbose, settings)
  , param
  , get
  , post
  , delete )
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Data.String (fromString)

import Model
import Actions (getPostsA, getPostByIdA, postPostA, deletePostByIdA)
import qualified Config (port, apiPrefix, verbose)

options :: IO Options
options = do
  verb <- Config.verbose
  port <- Config.port
  return $ Options
    { verbose = verb
    , settings = setPort port defaultSettings
    }

main :: IO ()
main = do
  doMigration
  prefix <- Config.apiPrefix
  opts <- options
  scottyOpts opts $ do
    get    (fromString (prefix ++ "post"))        getPostsA
    get    (fromString (prefix ++ "post/:i")) $ param "i" >>= getPostByIdA
    post   (fromString (prefix ++ "post"))        postPostA
    delete (fromString (prefix ++ "post/:i")) $ param "i" >>= deletePostByIdA
    --middleware $ basicAuth (verify) authSettings
    --Sc.post "/api/content"      $ postContent conn
