{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}

module Model
  ( doMigration
  , selectPosts
  , selectPostById
  , insertPost
  , validateUser
  , Post(..)
  , User(..) )
where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Database.Persist.Class
import Crypto.BCrypt
import Data.ByteString
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger   (runStderrLoggingT)
import GHC.Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post json
  title String
  html  String
  user UserId
  UniqueTitle title
  deriving Show
User
  username ByteString
  password ByteString
  UniqueUser username
  deriving Show
|]

runDb :: SqlPersistM a -> IO a
runDb query = do
  let connStr = "Database.db"
  runStderrLoggingT $ withSqlitePool connStr 10 $ \pool ->
    liftIO $ runSqlPersistMPool query pool

doMigration :: IO ()
doMigration = runDb (runMigration migrateAll)

selectPosts :: IO [Entity Post]
selectPosts = runDb $ selectList ([] :: [Filter Post]) []

selectPostById :: Integer -> IO (Maybe (Entity Post))
selectPostById index = runDb $ selectFirst [PostId ==. (toSqlKey . fromIntegral $ index)] []

validateUser :: User -> IO (Maybe Int64)
validateUser (User username password) = do
  user <- runDb $ selectFirst [UserUsername ==. username] []
  return $ (entityVal <$> user) >>= validateP >> (fromSqlKey . entityKey <$> user)
  where
    validateP user@(User _ passHash) =
      if validatePassword passHash password
      then Just $ username
      else Nothing

insertPost :: Post -> IO (Key Post)
insertPost = runDb . insert
