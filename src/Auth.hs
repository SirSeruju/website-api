{-# LANGUAGE OverloadedStrings #-}

module Auth (verify, authSettings) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.ByteString as Bs
import Data.ByteString.UTF8 as Bsu
import Crypto.BCrypt
import Network.Wai (Request, pathInfo, requestMethod)
import Network.Wai.Middleware.HttpAuth

data User = User Int String String deriving (Show)
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

verify :: Connection -> Bs.ByteString -> Bs.ByteString -> IO Bool
verify conn user password = do
  pwd <- queryNamed conn "SELECT * FROM users WHERE user = :user;" [":user" := Bsu.toString user] :: IO [User]
  case pwd of
    ((User _ _ passHash):[]) -> return $ validatePassword (Bsu.fromString passHash) password
    _                        -> return False


authSettings :: AuthSettings
authSettings = "My Realm" { authIsProtected = needsAuth }

needsAuth :: Request -> IO Bool
needsAuth req = do
  case (pathInfo req, show $ requestMethod req) of
    ("api":_, "POST") -> return True
    _                 -> return False
