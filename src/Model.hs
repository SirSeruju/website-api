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
  , deletePostById
  , validateUser
  , Post(..)
  , User(..) )
where

import Database.Persist.TH
  ( persistLowerCase
  , share
  , mkPersist
  , mkMigrate
  , sqlSettings )
import Database.Persist.Sqlite
  ( SqlPersistM
  , Entity
  , Filter
  , Key
  , insert
  , deleteWhere
  , entityVal
  , entityKey
  , toSqlKey
  , selectFirst
  , selectList
  , fromSqlKey
  , withSqlitePool
  , runSqlPersistMPool
  , runMigration
  , (==.))
import Crypto.BCrypt          (validatePassword)
import Data.ByteString        (ByteString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger   (runStderrLoggingT)
import GHC.Int                (Int64)

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
selectPostById index = runDb $
  selectFirst [PostId ==. (toSqlKey . fromIntegral $ index)] []

validateUser :: User -> IO (Maybe Int64)
validateUser (User username password) = do
  user <- runDb $ selectFirst [UserUsername ==. username] []
  return $ user >>= validateP . entityVal >> (fromSqlKey . entityKey <$> user)
  where
    validateP (User _ passHash) =
      if validatePassword passHash password
      then Just username
      else Nothing

insertPost :: Post -> IO (Key Post)
insertPost = runDb . insert

deletePostById :: Integer -> IO ()
deletePostById index = runDb $
  deleteWhere [PostId ==. (toSqlKey . fromIntegral $ index)]
