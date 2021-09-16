{-# LANGUAGE OverloadedStrings #-}
module Content (
  getContent, getContentById, postContent
) where

import Web.Scotty as Sc
import Network.HTTP.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson
import Data.Map as M
import Network.Wai.Middleware.HttpAuth
import Network.Wai
import Data.ByteString.UTF8 as Bsu
import Data.Text.Encoding
import Data.Text.Lazy

-- ContentField id user_id title content
data ContentField = ContentField Int Int String String deriving (Show)

instance FromRow ContentField where
  fromRow = ContentField <$> field <*> field <*> field <*> field

instance ToJSON ContentField where
  toJSON (ContentField id user title content) = object ["id" .= id, "user" .= user, "title" .= title, "content" .= content]
  toEncoding (ContentField id user title content) = pairs ("id" .= id <> "user" .= user <> "title" .= title <> "content" .= content)

-- User id username passwordHash
data User = User Int String String deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

-- ContentFieldWithUser id username title content
data ContentFieldWithUser = ContentFieldWithUser Int String String String deriving (Show)

instance FromRow ContentFieldWithUser where
  fromRow = ContentFieldWithUser <$> field <*> field <*> field <*> field

instance ToJSON ContentFieldWithUser where
  toJSON (ContentFieldWithUser id user title content) = object ["id" .= id, "user" .= user, "title" .= title, "content" .= content]
  toEncoding (ContentFieldWithUser id user title content) = pairs ("id" .= id <> "user" .= user <> "title" .= title <> "content" .= content)

getContent :: Connection -> Sc.ActionM ()
getContent conn = (Prelude.map fieldToJson) <$> fields >>= (\content -> Sc.json content)
  where
    fieldToJson (ContentFieldWithUser id user title content) = object ["id" .= id, "title" .= title, "user" .= user]
    fields :: Sc.ActionM [ContentFieldWithUser]
    fields = Sc.liftAndCatchIO $ query_ conn
      "SELECT t1.id, t2.user, t1.title, t1.content FROM content AS t1 \
      \LEFT JOIN users AS t2 ON t1.user_id = t2.id;"

getContentById :: String -> Connection -> Sc.ActionM ()
getContentById id conn = content
  where
    contents :: Sc.ActionM [ContentFieldWithUser]
    contents = Sc.liftAndCatchIO $ queryNamed conn
      "SELECT t1.id, t2.user, t1.title, t1.content FROM content AS t1 \
      \LEFT JOIN users AS t2 ON t1.user_id = t2.id \
      \WHERE t1.id = :id;" [":id" := id]
    content = contents >>=
      (\cs -> case cs of
        (c:_) -> Sc.json c
        []    -> Sc.raiseStatus status204 "No Content")


postContent :: Connection -> Sc.ActionM ()
postContent conn = do
  auth <- M.lookup "Authorization" . M.fromList <$> Sc.headers
  username <- case auth >>= extractBasicAuth . encodeUtf8 . toStrict of
    Just x -> return . Bsu.toString . fst $ x
    _      -> Sc.raiseStatus status401 "Unauthorized"
  users <- Sc.liftAndCatchIO $ queryNamed conn "SELECT * FROM users WHERE user = :user" [":user" := username] :: ActionM [User]
  User userId _ _ <- case users of
    (u:_) -> return u
    []    -> Sc.raiseStatus status401 "Unauthorized"
  
  body <- Sc.jsonData :: Sc.ActionM (M.Map String String)
  case ContentField
         <$> Just 0
         <*> Just userId
         <*> M.lookup "title" body
         <*> M.lookup "content" body of
    Just (ContentField _ user title content) ->
      Sc.liftAndCatchIO $ executeNamed conn
        "INSERT INTO content(user_id, title, content) VALUES (:user_id, :title, :content)"
        [ ":user_id" := userId, ":title" := title, ":content" := content ]
    _                                        -> Sc.raiseStatus status400 "Required fields: title, content"
