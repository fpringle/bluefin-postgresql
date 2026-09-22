{-# LANGUAGE CPP #-}

module Bluefin.PostgreSQL
  ( -- * Effects
    WithConnection
  , withConnection
  , PostgreSQL

    -- ** Interpreters
  , runWithConnection

#if POOL
  , runWithConnectionPool
#endif

  , runPostgreSQL
#if OTEL
  , runPostgreSQLOT
#endif

    -- * Lifted versions of functions from Database.PostgreSQL.Simple

    -- ** Queries that return results
  , query
  , query_
  , queryWith
  , queryWith_

    -- ** Statements that do not return results
  , execute
  , execute_
  , executeMany

    -- ** Transaction handling
  , withTransaction
  , withSavepoint
  , begin
  , commit
  , rollback

    -- ** Queries that stream results
  , fold
  , foldWithOptions
  , fold_
  , foldWithOptions_
  , forEach
  , forEach_
  , returning
  , foldWith
  , foldWithOptionsAndParser
  , foldWith_
  , foldWithOptionsAndParser_
  , forEachWith
  , forEachWith_
  , returningWith
  )
where

import Bluefin.PostgreSQL.Connection as Conn
import Bluefin.PostgreSQL.Effect
#if POOL
import Bluefin.PostgreSQL.Connection.Pool as Pool
#endif
