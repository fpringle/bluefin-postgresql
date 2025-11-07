{-# LANGUAGE CPP #-}

module Bluefin.PostgreSQL
  ( -- * Effect
    WithConnection
  , withConnection

    -- ** Interpreters
  , runWithConnection

#if POOL
  , runWithConnectionPool
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

import Data.Int (Int64)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.FromRow as PSQL
import Bluefin.Eff
import Bluefin.IO
import Bluefin.PostgreSQL.Connection as Conn
import GHC.Stack
#if POOL
import Bluefin.PostgreSQL.Connection.Pool as Pool
#endif

-- | Lifted 'PSQL.query'.
query ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow q, PSQL.FromRow r) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  q ->
  Eff es [r]
query wc ioe q row = withConnection wc $ \conn -> effIO ioe (PSQL.query conn q row)


-- | Lifted 'PSQL.query_'.
query_ ::
  (HasCallStack, e :> es, e1 :> es, PSQL.FromRow r) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  Eff es [r]
query_ wc ioe row = withConnection wc $ \conn -> effIO ioe (PSQL.query_ conn row)

-- | Lifted 'PSQL.queryWith'.
queryWith ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow q) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.RowParser r ->
  PSQL.Query ->
  q ->
  Eff es [r]
queryWith wc ioe parser q row =
  withConnection wc $ \conn -> effIO ioe (PSQL.queryWith parser conn q row)

-- | Lifted 'PSQL.queryWith_'.
queryWith_ ::
  (HasCallStack, e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.RowParser r ->
  PSQL.Query ->
  Eff es [r]
queryWith_ wc ioe parser row =
  withConnection wc $ \conn -> effIO ioe (PSQL.queryWith_ parser conn row)

-- | Lifted 'PSQL.execute'.
execute ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow q) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  q ->
  Eff es Int64
execute wc ioe q row = withConnection wc $ \conn -> effIO ioe (PSQL.execute conn q row)

-- | Lifted 'PSQL.execute_'.
execute_ ::
  (HasCallStack, e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  Eff es Int64
execute_ wc ioe row = withConnection wc $ \conn -> effIO ioe (PSQL.execute_ conn row)

-- | Lifted 'PSQL.executeMany'.
executeMany ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow q) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  [q] ->
  Eff es Int64
executeMany wc ioe q rows = withConnection wc $ \conn -> effIO ioe (PSQL.executeMany conn q rows)

-- | Lifted 'PSQL.withTransaction'.
withTransaction ::
  (HasCallStack, e :> es, e1 :> es) => 
  WithConnection e ->
  IOE e1 ->
  Eff es a -> 
  Eff es a
withTransaction wc ioe f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.withTransaction conn (unlift f)

-- | Lifted 'PSQL.withSavepoint'.
withSavepoint ::
  (HasCallStack, e :> es, e1 :> es) => 
  WithConnection e ->
  IOE e1 ->
  Eff es a -> Eff es a
withSavepoint wc ioe f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.withSavepoint conn (unlift f)

-- | Lifted 'PSQL.begin'.
begin ::
  (HasCallStack, e :> es, e1 :> es) => 
  WithConnection e ->
  IOE e1 ->
  Eff es ()
begin wc ioe = withConnection wc $ effIO ioe . PSQL.begin

-- | Lifted 'PSQL.commit'.
commit ::
  (HasCallStack, e :> es, e1 :> es) => 
  WithConnection e ->
  IOE e1 ->
  Eff es ()
commit wc ioe = withConnection wc $ effIO ioe . PSQL.commit

-- | Lifted 'PSQL.rollback'.
rollback ::
  (HasCallStack, e :> es, e1 :> es) => 
  WithConnection e ->
  IOE e1 ->
  Eff es ()
rollback wc ioe = withConnection wc $ effIO ioe . PSQL.rollback

(...) :: (a -> b) -> (t1 -> t2 -> a) -> t1 -> t2 -> b
unlift ... f = \a' row -> unlift $ f a' row

unliftWithConn ::
  (HasCallStack, e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  (PSQL.Connection -> (forall b. Eff es b -> IO b) -> IO a) ->
  Eff es a
unliftWithConn wc ioe f =
  withConnection wc $ \conn ->
    withEffToIO_ ioe $ \unlift ->
      f conn unlift
{-# INLINE unliftWithConn #-}

-- | Lifted 'PSQL.fold'.
fold ::
  (HasCallStack, e :> es, e1 :> es, PSQL.FromRow row, PSQL.ToRow params) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
fold wc ioe q params a f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.fold conn q params a (unlift ... f)

-- | Lifted 'PSQL.foldWithOptions'.
foldWithOptions ::
  (HasCallStack, e :> es, e1 :> es, PSQL.FromRow row, PSQL.ToRow params) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.FoldOptions ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptions wc ioe opts q params a f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.foldWithOptions opts conn q params a (unlift ... f)

-- | Lifted 'PSQL.fold_'.
fold_ ::
  (HasCallStack, e :> es, e1 :> es, PSQL.FromRow row) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
fold_ wc ioe q a f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.fold_ conn q a (unlift ... f)

-- | Lifted 'PSQL.foldWithOptions_'.
foldWithOptions_ ::
  (HasCallStack, e :> es, e1 :> es, PSQL.FromRow row) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.FoldOptions ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptions_ wc ioe opts q a f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.foldWithOptions_ opts conn q a (unlift ... f)

-- | Lifted 'PSQL.forEach'.
forEach ::
  (HasCallStack, e :> es, e1 :> es, PSQL.FromRow r, PSQL.ToRow q) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  q ->
  (r -> Eff es ()) ->
  Eff es ()
forEach wc ioe q row forR =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.forEach conn q row (unlift . forR)

-- | Lifted 'PSQL.forEach_'.
forEach_ ::
  (HasCallStack, e :> es, e1 :> es, PSQL.FromRow r) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  (r -> Eff es ()) ->
  Eff es ()
forEach_ wc ioe q forR =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.forEach_ conn q (unlift . forR)

-- | Lifted 'PSQL.returning'.
returning ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow q, PSQL.FromRow r) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.Query ->
  [q] ->
  Eff es [r]
returning wc ioe q rows = withConnection wc $ \conn -> effIO ioe $ PSQL.returning conn q rows

-- | Lifted 'PSQL.foldWith'.
foldWith ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow params) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.RowParser row ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWith wc ioe parser q params a f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.foldWith parser conn q params a (unlift ... f)

-- | Lifted 'PSQL.foldWithOptionsAndParser'.
foldWithOptionsAndParser ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow params) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.FoldOptions ->
  PSQL.RowParser row ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptionsAndParser wc ioe opts parser q params a f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.foldWithOptionsAndParser opts parser conn q params a (unlift ... f)

-- | Lifted 'PSQL.foldWith_'.
foldWith_ ::
  (HasCallStack, e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.RowParser row ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWith_ wc ioe parser q a f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.foldWith_ parser conn q a (unlift ... f)

-- | Lifted 'PSQL.foldWithOptionsAndParser_'.
foldWithOptionsAndParser_ ::
  (HasCallStack, e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.FoldOptions ->
  PSQL.RowParser row ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptionsAndParser_ wc ioe opts parser q a f =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.foldWithOptionsAndParser_ opts parser conn q a (unlift ... f)

-- | Lifted 'PSQL.forEachWith'.
forEachWith ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow q) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.RowParser r ->
  PSQL.Query ->
  q ->
  (r -> Eff es ()) ->
  Eff es ()
forEachWith wc ioe parser q row forR =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.forEachWith parser conn q row (unlift . forR)

-- | Lifted 'PSQL.forEachWith_'.
forEachWith_ ::
  (HasCallStack, e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.RowParser r ->
  PSQL.Query ->
  (r -> Eff es ()) ->
  Eff es ()
forEachWith_ wc ioe parser row forR =
  unliftWithConn wc ioe $ \conn unlift ->
    PSQL.forEachWith_ parser conn row (unlift . forR)

-- | Lifted 'PSQL.returningWith'.
returningWith ::
  (HasCallStack, e :> es, e1 :> es, PSQL.ToRow q) =>
  WithConnection e ->
  IOE e1 ->
  PSQL.RowParser r ->
  PSQL.Query ->
  [q] ->
  Eff es [r]
returningWith wc ioe parser q rows =
  withConnection wc $ \conn -> effIO ioe $ PSQL.returningWith parser conn q rows
