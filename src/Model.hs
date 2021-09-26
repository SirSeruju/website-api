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
  ( doMigration, getPosts )
where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.ByteString
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger   (runStderrLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post json
  title String
  html  String
  user UserId
  UniqueTitle title
  deriving Show
User
  user String
  password ByteString
  UniqueUser user
  deriving Show
|]

runDb :: SqlPersistM a -> IO a
runDb query = do
  let connStr = "Database.db"
  runStderrLoggingT $ withSqlitePool connStr 10 $ \pool ->
    liftIO $ runSqlPersistMPool query pool

doMigration :: IO ()
doMigration = runDb (runMigration migrateAll)

getPosts :: IO [Entity Post]
getPosts = runDb $ selectList ([] :: [Filter Post]) []
