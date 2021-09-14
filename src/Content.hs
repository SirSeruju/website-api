{-# LANGUAGE OverloadedStrings #-}
module Content (
  getContent, getContentById, postContent
) where

import Web.Scotty as Sc
import Network.HTTP.Types
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson

data ContentField = ContentField Int String String deriving (Show)

instance FromRow ContentField where
  fromRow = ContentField <$> field <*> field <*> field

instance ToJSON ContentField where
  toJSON (ContentField id name html) = object ["id" .= id, "name" .= name, "html" .= html]
  toEncoding (ContentField id name html) = pairs ("id" .= id <> "name" .= name <> "html" .= html)

getContent :: Connection -> Sc.ActionM ()
getContent conn = (Prelude.map fieldToJson) <$> fields >>= (\content -> Sc.json content)
  where
    fieldToJson (ContentField id name _) = object ["id" .= id, "name" .= name]
    fields :: Sc.ActionM [ContentField]
    fields = Sc.liftAndCatchIO . query_ conn $ "SELECT * FROM content;"

getContentById :: String -> Connection -> Sc.ActionM ()
getContentById id conn = field >>= (\content -> Sc.json content)
  where
    field :: Sc.ActionM ContentField
    field = do
      fields <- Sc.liftAndCatchIO $ queryNamed conn "SELECT * FROM content WHERE id = :id;" [":id" := id]
      case fields of
        []     -> raiseStatus status204 "No Content"
        (x:xs) -> return x

postContent :: Connection -> Sc.ActionM ()
postContent conn = undefined
