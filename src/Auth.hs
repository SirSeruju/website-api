{-# LANGUAGE OverloadedStrings #-}

module Auth (verifyCredentials) where

import Web.Scotty as Sc
import Network.HTTP.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson
import Data.ByteString as B
import Crypto.BCrypt

data User = User Int String ByteString deriving (Show)
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

verifyCredentials :: B.ByteString -> B.ByteString -> Connection -> Sc.ActionM ()
verifyCredentials user password conn = do
  pwd <- Sc.liftAndCatchIO $ queryNamed conn "SELECT * FROM users WHERE user = :user;" [":user" := user] :: Sc.ActionM [User]
  case pwd of
    ((User id _ passHash):[]) ->
      if   validatePassword passHash password
      then Sc.json $ id
      else raiseStatus status401 "Unauthorized"
    _  -> raiseStatus status401 "Unauthorized"
