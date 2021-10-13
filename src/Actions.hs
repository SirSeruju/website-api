{-# LANGUAGE OverloadedStrings #-}
module Actions
  ( getPostsA
  , getPostByIdA
  , postPostA
  , loginA
  , noContentA
  , unauthorizedA )
where
  

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
import Data.Functor ((<&>))

import Auth
import Model

getPostsA :: ActionM ()
getPostsA = do
  posts <- liftAndCatchIO selectPosts
  json $ object ["posts" .= posts]

getPostByIdA :: Integer -> ActionM ()
getPostByIdA index = do
  post <- liftAndCatchIO $ selectPostById index
  maybe noContentA json post

postPostA :: ActionM ()
postPostA = do
  userId <- loginA
  post <- (jsonData :: ActionM (HashMap Text Value)) <&> insert "user" (Number . fromIntegral $ userId)
  liftAndCatchIO ((catchResult . fromJSON . toJSON $ post) >>= insertPost) >>= json
  where
    catchResult :: Result a -> IO a
    catchResult (Error err) = error err
    catchResult (Success a) = return a
  
loginA :: ActionM Int64
loginA = do
  req <- request
  case Prelude.lookup hAuthorization (requestHeaders req) >>= extractBasicAuth of
    Just (username, password) -> do
      username <- liftAndCatchIO . validateUser $ User username password
      maybe unauthorizedA return username

noContentA :: ActionM a
noContentA = raiseStatus noContent204 "No content."

unauthorizedA :: ActionM a
unauthorizedA = raiseStatus unauthorized401 "Basic Authorization needed."

