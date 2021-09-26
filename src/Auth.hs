{-# LANGUAGE OverloadedStrings #-}

module Auth (authSettings) where

import Crypto.BCrypt
import Network.Wai (Request, pathInfo, requestMethod)
import Network.Wai.Middleware.HttpAuth

authSettings :: AuthSettings
authSettings = "My Realm" { authIsProtected = needsAuth }

needsAuth :: Request -> IO Bool
needsAuth req = do
  case (pathInfo req, show $ requestMethod req) of
    ("api":_, "POST") -> return True
    _                 -> return False
