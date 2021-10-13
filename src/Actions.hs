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
  ( ActionM
  , raiseStatus
  , request
  , liftAndCatchIO
  , json
  , jsonData )
import Data.Aeson
  ( object
  , (.=)
  , Value (Number)
  , fromJSON
  , toJSON
  , Result(..))
import Network.HTTP.Types (hAuthorization, unauthorized401)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import Network.Wai (requestHeaders)
import Network.HTTP.Types.Status (noContent204)
import GHC.Int (Int64)
import Data.HashMap.Strict (HashMap, insert)
import Data.Text (Text)
import Data.Functor ((<&>))

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
      validUsername <- liftAndCatchIO . validateUser $ User username password
      maybe unauthorizedA return validUsername
    Nothing -> unauthorizedA

noContentA :: ActionM a
noContentA = raiseStatus noContent204 "No content."

unauthorizedA :: ActionM a
unauthorizedA = raiseStatus unauthorized401 "Basic Authorization needed."

