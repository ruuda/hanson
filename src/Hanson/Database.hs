{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hanson.Database
  ( Transaction
  , newConnectionPool
  , withConnection
  , withTransaction
  , withPoolTransaction
  , commit
  , rollback
  , query
  )
where

import Control.Concurrent.MVar (MVar)
import Control.Exception (bracket)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Database.PostgreSQL.Simple.Transaction (ReadWriteMode, IsolationLevel (..), TransactionMode (TransactionMode))

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple.Transaction as Postgres

newConnectionPool :: IO (Pool Connection)
newConnectionPool =
  let
    numStripes = 1
    idleTimeSeconds = 300 :: Int
    maxConnections = 10
  in
    Pool.createPool
      -- We connect using an empty connection string, relying on the PG*
      -- environment variables to provide the connection details instead.
      (connectPostgreSQL "")
      (Postgres.close)
      numStripes
      (fromIntegral idleTimeSeconds)
      maxConnections

withConnection
  :: Pool Connection
  -> (Connection -> IO a)
  -> IO a
withConnection = Pool.withResource

-- A transaction wraps a connection, it doesn't do anything execpt signal in the
-- type system that we expect something to be run inside a transaction. Most
-- functions accept a transaction instead of a connection. We use an MVar so we
-- can clear it after commit or rollback, to prevent mis-use. Linear types would
-- be even nicer, but let's not go there right now.
newtype Transaction = Transaction (MVar Connection)

-- Begin a new transaction with serializable isolation level.
begin :: ReadWriteMode -> Connection -> IO Transaction
begin readWriteMode conn = do
  Postgres.beginMode (TransactionMode Serializable readWriteMode) conn
  mvar <- MVar.newMVar conn
  pure $ Transaction mvar

commit :: Transaction -> IO ()
commit (Transaction connVar) = MVar.tryTakeMVar connVar >>= \case
  Just conn -> Postgres.commit conn
  Nothing   -> error "Trying to commit a closed transaction."

rollback :: Transaction -> IO ()
rollback (Transaction connVar) = MVar.tryTakeMVar connVar >>= \case
  Just conn -> Postgres.rollback conn
  Nothing   -> error "Trying to roll back a closed transaction."

-- Roll back the transaction if it is not already closed.
ensureClosed :: Transaction -> IO ()
ensureClosed (Transaction connVar) = MVar.tryTakeMVar connVar >>= \case
  Just conn -> Postgres.rollback conn
  Nothing   -> pure ()

-- Run f in a transaction. If f throws, the transaction is rolled back. If after
-- f returns the transaction is not manually closed (by an explicit commit or
-- rollback), the transaction is rolled back as well.
withTransaction :: ReadWriteMode -> Connection -> (Transaction -> IO a) -> IO a
withTransaction readWriteMode conn f = bracket
  (begin readWriteMode conn)
  (ensureClosed)
  f

-- Combines withTransaction and withConnection.
withPoolTransaction :: ReadWriteMode -> Pool Connection -> (Transaction -> IO a) -> IO a
withPoolTransaction readWriteMode pool f =
  withConnection pool $ \conn ->
    withTransaction readWriteMode conn f

query
  :: (Postgres.ToRow q, Postgres.FromRow r)
  => Transaction
  -> Postgres.Query
  -> q
  -> IO [r]
query (Transaction connVar) template qs = MVar.tryReadMVar connVar >>= \case
  Just conn -> Postgres.query conn template qs
  Nothing   -> error "Trying to query a closed transaction."
