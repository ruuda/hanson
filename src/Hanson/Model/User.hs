{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hanson.Model.User
  ( User (..)
  , getById
  , getByUsername
  )
where

import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (FromRow, Only (..))

import Hanson.Database (Transaction)

import qualified Hanson.Database as Db

data User = User
  { userId :: !Int64
  , userUsername :: !Text
  , userFullName :: !Text
  , userCreatedAt :: !UTCTime
  } deriving (Eq, Show, Generic, FromRow)

getById :: Transaction -> Int64 -> IO (Maybe User)
getById tx id = do
  result <- Db.query tx [sql|
      SELECT
        id,
        user_current_username(id),
        user_current_full_name(id),
        created_at
      FROM
        "user"
      WHERE
        id = ?;
    |] [id]
  case result of
    [user] -> pure $ Just user
    _      -> pure Nothing

getByUsername :: Transaction -> Text -> IO (Maybe User)
getByUsername tx username = do
  result <- Db.query tx [sql|
      SELECT user_id
      FROM user_username
      WHERE username = ?
    |] [username]
  case result of
    [Only userId] -> getById tx userId
    _             -> pure Nothing
