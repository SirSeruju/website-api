{-# LANGUAGE OverloadedStrings #-}

module Auth (verify) where

import Web.Scotty as Sc
import Network.HTTP.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson
import Data.ByteString as Bs
import Data.ByteString.UTF8 as Bsu
import Data.Map as M
import Crypto.BCrypt

data User = User Int String String deriving (Show)
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

verify :: Connection -> Sc.ActionM ()
verify conn = do
  body <- Sc.jsonData :: Sc.ActionM (M.Map String String)
  case (,) <$> (M.lookup "user" body) <*> (M.lookup "password" body) of
    Just (user, password) -> verifyCredentials user password conn
    _                     -> raiseStatus status401 "Unauthorized"

verifyCredentials :: String -> String -> Connection -> Sc.ActionM ()
verifyCredentials user password conn = do
  pwd <- Sc.liftAndCatchIO $ queryNamed conn "SELECT * FROM users WHERE user = :user;" [":user" := user] :: Sc.ActionM [User]
  case pwd of
    ((User id _ passHash):[]) ->
      if   validatePassword (Bsu.fromString passHash) (Bsu.fromString password)
      then Sc.json id
      else raiseStatus status401 "Unauthorized"
    _                         -> raiseStatus status401 "Unauthorized"
