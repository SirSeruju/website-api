{-# LANGUAGE OverloadedStrings #-}
module Actions
  ( getPostsA
  , getPostByIdA
  , postPostA
  , deletePostByIdA
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
import Network.HTTP.Types
  ( hAuthorization
  , unauthorized401
  , notFound404
  , methodNotAllowed405 )
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import Network.Wai (requestHeaders)
import Network.HTTP.Types.Status (noContent204)
import GHC.Int (Int64)
import Data.HashMap.Strict (HashMap, insert)
import Data.Text (Text)
import Data.Functor ((<&>))
import Database.Persist.Sqlite (entityVal, fromSqlKey)

import Model

getPostsA :: ActionM ()
getPostsA = do
  posts <- liftAndCatchIO selectPosts
  json $ object ["posts" .= posts]

getPostByIdA :: Integer -> ActionM ()
getPostByIdA index = do
  post <- liftAndCatchIO $ selectPostById index
  maybe noContentA json post

deletePostByIdA :: Integer -> ActionM ()
deletePostByIdA index = do
  userId <- loginA
  post   <- liftAndCatchIO $ selectPostById index
  case post of
    Nothing -> notFoundA
    Just p  ->
      if   (fromSqlKey . postUser. entityVal $ p) == userId
      then liftAndCatchIO $ deletePostById index
      else methodNotAllowedA

postPostA :: ActionM ()
postPostA = do
  userId <- loginA
  post <- (jsonData :: ActionM (HashMap Text Value))
    <&> insert "user" (Number . fromIntegral $ userId)
  liftAndCatchIO ((catchResult . fromJSON . toJSON $ post) >>= insertPost)
    >>= (\i -> json $ object ["id" .= i])
  where
    catchResult :: Result a -> IO a
    catchResult (Error err) = error err
    catchResult (Success a) = return a
  
loginA :: ActionM Int64
loginA = do
  req <- request
  case lookup hAuthorization (requestHeaders req) >>= extractBasicAuth of
    Just (username, password) -> do
      validUsername <- liftAndCatchIO . validateUser $ User username password
      maybe methodNotAllowedA return validUsername
    Nothing -> unauthorizedA

noContentA :: ActionM a
noContentA = raiseStatus noContent204 "No content."

methodNotAllowedA :: ActionM a
methodNotAllowedA = raiseStatus methodNotAllowed405 "Method not allowed."

notFoundA :: ActionM a
notFoundA = raiseStatus notFound404 "Not found."

unauthorizedA :: ActionM a
unauthorizedA = raiseStatus unauthorized401 "Basic Authorization needed."

