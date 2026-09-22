{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Bluefin.PostgreSQL.Effect
  ( -- * Effect
    PostgreSQL (..)

    -- ** Interpreters
  , runPostgreSQL

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
  , withTransactionLevel
  , PSQL.IsolationLevel (..)
  , withTransactionMode
  , PSQL.TransactionMode (..)
  , PSQL.ReadWriteMode (..)
  , withTransactionModeRetry
  , withTransactionModeRetry'
  , withTransactionSerializable
  , withTransactionSerialisable
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

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.PostgreSQL.Connection
import qualified Control.Exception as E
import Data.Int (Int64)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.FromRow as PSQL
import qualified Database.PostgreSQL.Simple.Transaction as PSQL
import GHC.Stack

-- | Dynamic effect representing all the Postgres operations we want to perform.
data PostgreSQL (e :: Effects) = MkPostgreSQL
  { queryImpl :: forall q r. (PSQL.ToRow q, PSQL.FromRow r) => PSQL.Query -> q -> Eff e [r]
  -- ^ Lifted 'PSQL.query'.
  , queryWithImpl :: forall q r. (PSQL.ToRow q) => PSQL.RowParser r -> PSQL.Query -> q -> Eff e [r]
  -- ^ Lifted 'PSQL.queryWith'.
  , query_Impl :: forall r. (PSQL.FromRow r) => PSQL.Query -> Eff e [r]
  -- ^ Lifted 'PSQL.query_'.
  , queryWith_Impl :: forall r. PSQL.RowParser r -> PSQL.Query -> Eff e [r]
  -- ^ Lifted 'PSQL.queryWith_'.
  , --

    executeImpl :: forall q. (PSQL.ToRow q) => PSQL.Query -> q -> Eff e Int64
  -- ^ Lifted 'PSQL.execute'.
  , execute_Impl :: PSQL.Query -> Eff e Int64
  -- ^ Lifted 'PSQL.execute_'.
  , executeManyImpl :: forall q. (PSQL.ToRow q) => PSQL.Query -> [q] -> Eff e Int64
  -- ^ Lifted 'PSQL.executeMany'.
  , --

    withTransactionImpl :: forall e' a. Eff e' a -> Eff (e' :& e) a
  -- ^ Lifted 'PSQL.withTransaction'.
  , withTransactionLevelImpl :: forall e' a. PSQL.IsolationLevel -> Eff e' a -> Eff (e' :& e) a
  -- ^ Lifted 'PSQL.withTransactionLevel'.
  , withTransactionModeImpl :: forall e' a. PSQL.TransactionMode -> Eff e' a -> Eff (e' :& e) a
  -- ^ Lifted 'PSQL.withTransactionMode'.
  , withTransactionModeRetryImpl :: forall e' a. PSQL.TransactionMode -> (PSQL.SqlError -> Bool) -> Eff e' a -> Eff (e' :& e) a
  -- ^ Lifted 'PSQL.withTransactionModeRetry'.
  , withTransactionModeRetry'Impl :: forall exc e' a. (E.Exception exc) => PSQL.TransactionMode -> (exc -> Bool) -> Eff e' a -> Eff (e' :& e) a
  -- ^ Lifted 'PSQL.withTransactionModeRetry''.
  , withTransactionSerializableImpl :: forall e' a. Eff e' a -> Eff (e' :& e) a
  -- ^ Lifted 'PSQL.withTransactionSerializable'.
  , withSavepointImpl :: forall e' a. Eff e' a -> Eff (e' :& e) a
  -- ^ Lifted 'PSQL.withSavepoint'.
  , beginImpl :: Eff e ()
  -- ^ Lifted 'PSQL.begin'.
  , commitImpl :: Eff e ()
  -- ^ Lifted 'PSQL.commit'.
  , rollbackImpl :: Eff e ()
  -- ^ Lifted 'PSQL.rollback'.
  , --

    foldImpl ::
      forall row params e' a.
      (PSQL.FromRow row, PSQL.ToRow params) =>
      PSQL.Query ->
      params ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& e) a
  -- ^ Lifted 'PSQL.fold'.
  , fold_Impl ::
      forall row e' a.
      (PSQL.FromRow row) =>
      PSQL.Query ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& e) a
  -- ^ Lifted 'PSQL.fold_'.
  , foldWithOptionsImpl ::
      forall row params e' a.
      (PSQL.FromRow row, PSQL.ToRow params) =>
      PSQL.FoldOptions ->
      PSQL.Query ->
      params ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& e) a
  -- ^ Lifted 'PSQL.foldWithOptions'.
  , foldWithOptions_Impl ::
      forall row e' a.
      (PSQL.FromRow row) =>
      PSQL.FoldOptions ->
      PSQL.Query ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& e) a
  -- ^ Lifted 'PSQL.foldWithOptions_'.
  , forEachImpl ::
      forall r q e'.
      (PSQL.FromRow r, PSQL.ToRow q) =>
      PSQL.Query ->
      q ->
      (r -> Eff e' ()) ->
      Eff (e' :& e) ()
  -- ^ Lifted 'PSQL.forEach'.
  , forEach_Impl ::
      forall r e'.
      (PSQL.FromRow r) =>
      PSQL.Query ->
      (r -> Eff e' ()) ->
      Eff (e' :& e) ()
  -- ^ Lifted 'PSQL.forEach_'.
  , returningImpl ::
      forall r q.
      (PSQL.ToRow q, PSQL.FromRow r) =>
      PSQL.Query ->
      [q] ->
      Eff e [r]
  -- ^ Lifted 'PSQL.returning'.
  , foldWithImpl ::
      forall row params e' a.
      (PSQL.ToRow params) =>
      PSQL.RowParser row ->
      PSQL.Query ->
      params ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& e) a
  -- ^ Lifted 'PSQL.foldWith'.
  , foldWithOptionsAndParserImpl ::
      forall row params e' a.
      (PSQL.ToRow params) =>
      PSQL.FoldOptions ->
      PSQL.RowParser row ->
      PSQL.Query ->
      params ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& e) a
  -- ^ Lifted 'PSQL.foldWithOptionsAndParser'.
  , foldWith_Impl ::
      forall row e' a.
      () =>
      PSQL.RowParser row ->
      PSQL.Query ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& e) a
  -- ^ Lifted 'PSQL.foldWith_'.
  , foldWithOptionsAndParser_Impl ::
      forall row e' a.
      () =>
      PSQL.FoldOptions ->
      PSQL.RowParser row ->
      PSQL.Query ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& e) a
  -- ^ Lifted 'PSQL.foldWithOptionsAndParser_'.
  , forEachWithImpl ::
      forall r q e'.
      (PSQL.ToRow q) =>
      PSQL.RowParser r ->
      PSQL.Query ->
      q ->
      (r -> Eff e' ()) ->
      Eff (e' :& e) ()
  -- ^ Lifted 'PSQL.forEachWith'.
  , forEachWith_Impl ::
      forall r e'.
      () =>
      PSQL.RowParser r ->
      PSQL.Query ->
      (r -> Eff e' ()) ->
      Eff (e' :& e) ()
  -- ^ Lifted 'PSQL.forEachWith_'.
  , returningWithImpl ::
      forall r q.
      (PSQL.ToRow q) =>
      PSQL.RowParser r ->
      PSQL.Query ->
      [q] ->
      Eff e [r]
  -- ^ Lifted 'PSQL.returningWith'.
  }
  deriving (Handle) via OneWayCoercibleHandle PostgreSQL

mapHandlePostgreSQL :: (e :> es) => PostgreSQL e -> PostgreSQL es
mapHandlePostgreSQL psql =
  MkPostgreSQL
    { queryImpl = \q row -> useImpl (queryImpl psql q row)
    , queryWithImpl = \parser q row -> useImpl (queryWithImpl psql parser q row)
    , query_Impl = useImpl . query_Impl psql
    , queryWith_Impl = \parser q -> useImpl (queryWith_Impl psql parser q)
    , executeImpl = \q row -> useImpl (executeImpl psql q row)
    , execute_Impl = useImpl . execute_Impl psql
    , executeManyImpl = \parser qs -> useImpl (executeManyImpl psql parser qs)
    , withTransactionImpl = useImplUnder . withTransactionImpl psql
    , withTransactionLevelImpl = \level -> useImplUnder . withTransactionLevelImpl psql level
    , withTransactionModeImpl = \mode -> useImplUnder . withTransactionModeImpl psql mode
    , withTransactionModeRetryImpl = \mode shouldRetry -> useImplUnder . withTransactionModeRetryImpl psql mode shouldRetry
    , withTransactionModeRetry'Impl = \mode shouldRetry -> useImplUnder . withTransactionModeRetry'Impl psql mode shouldRetry
    , withTransactionSerializableImpl = useImplUnder . withTransactionSerializableImpl psql
    , withSavepointImpl = useImplUnder . withSavepointImpl psql
    , beginImpl = useImpl (beginImpl psql)
    , commitImpl = useImpl (commitImpl psql)
    , rollbackImpl = useImpl (rollbackImpl psql)
    , foldImpl = \q params a f -> useImplUnder (foldImpl psql q params a f)
    , fold_Impl = \q a f -> useImplUnder (fold_Impl psql q a f)
    , foldWithOptionsImpl = \opts q params a f -> useImplUnder (foldWithOptionsImpl psql opts q params a f)
    , foldWithOptions_Impl = \opts q a f -> useImplUnder (foldWithOptions_Impl psql opts q a f)
    , forEachImpl = \q row f -> useImplUnder (forEachImpl psql q row f)
    , forEach_Impl = \q f -> useImplUnder (forEach_Impl psql q f)
    , returningImpl = \q rows -> useImpl (returningImpl psql q rows)
    , foldWithImpl = \parser q params a f -> useImplUnder (foldWithImpl psql parser q params a f)
    , foldWithOptionsAndParserImpl = \opts parser q params a f -> useImplUnder (foldWithOptionsAndParserImpl psql opts parser q params a f)
    , foldWith_Impl = \parser params a f -> useImplUnder (foldWith_Impl psql parser params a f)
    , foldWithOptionsAndParser_Impl = \opts parser params a f -> useImplUnder (foldWithOptionsAndParser_Impl psql opts parser params a f)
    , forEachWithImpl = \parser q row f -> useImplUnder (forEachWithImpl psql parser q row f)
    , forEachWith_Impl = \parser row f -> useImplUnder (forEachWith_Impl psql parser row f)
    , returningWithImpl = \parser q rows -> useImpl (returningWithImpl psql parser q rows)
    }

instance (e :> es) => OneWayCoercible (PostgreSQL e) (PostgreSQL es) where
  oneWayCoercibleImpl = oneWayCoercibleTrustMe mapHandlePostgreSQL

-- | Lifted 'PSQL.query'.
query ::
  forall q r e es.
  (HasCallStack, e :> es, PSQL.ToRow q, PSQL.FromRow r) =>
  PostgreSQL e ->
  PSQL.Query ->
  q ->
  Eff es [r]
query psql q row = makeOp (queryImpl (mapHandle psql) q row)

-- | Lifted 'PSQL.query_'.
query_ ::
  (HasCallStack, e :> es, PSQL.FromRow r) =>
  PostgreSQL e ->
  PSQL.Query ->
  Eff es [r]
query_ psql row = makeOp (query_Impl (mapHandle psql) row)

-- | Lifted 'PSQL.queryWith'.
queryWith ::
  (HasCallStack, e :> es, PSQL.ToRow q) =>
  PostgreSQL e ->
  PSQL.RowParser r ->
  PSQL.Query ->
  q ->
  Eff es [r]
queryWith psql parser q row = makeOp (queryWithImpl (mapHandle psql) parser q row)

-- | Lifted 'PSQL.queryWith_'.
queryWith_ ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  PSQL.RowParser r ->
  PSQL.Query ->
  Eff es [r]
queryWith_ psql parser row = makeOp (queryWith_Impl (mapHandle psql) parser row)

-- | Lifted 'PSQL.execute'.
execute ::
  (HasCallStack, e :> es, PSQL.ToRow q) =>
  PostgreSQL e ->
  PSQL.Query ->
  q ->
  Eff es Int64
execute psql q row = makeOp (executeImpl (mapHandle psql) q row)

-- | Lifted 'PSQL.execute_'.
execute_ ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  PSQL.Query ->
  Eff es Int64
execute_ psql row = makeOp (execute_Impl (mapHandle psql) row)

-- | Lifted 'PSQL.executeMany'.
executeMany ::
  (HasCallStack, e :> es, PSQL.ToRow q) =>
  PostgreSQL e ->
  PSQL.Query ->
  [q] ->
  Eff es Int64
executeMany psql q rows = makeOp (executeManyImpl (mapHandle psql) q rows)

-- | Lifted 'PSQL.withTransaction'.
withTransaction ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  Eff es a ->
  Eff es a
withTransaction psql f = makeOp (withTransactionImpl (mapHandle psql) f)

-- | Lifted 'PSQL.withTransactionLevel'.
withTransactionLevel ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  PSQL.IsolationLevel ->
  Eff es a ->
  Eff es a
withTransactionLevel psql level f = makeOp (withTransactionLevelImpl (mapHandle psql) level f)

-- | Lifted 'PSQL.withTransactionMode'.
withTransactionMode ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  PSQL.TransactionMode ->
  Eff es a ->
  Eff es a
withTransactionMode psql mode f = makeOp (withTransactionModeImpl (mapHandle psql) mode f)

-- | Lifted 'PSQL.withTransactionMode'.
withTransactionModeRetry ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  PSQL.TransactionMode ->
  (PSQL.SqlError -> Bool) ->
  Eff es a ->
  Eff es a
withTransactionModeRetry psql mode shouldRetry f = makeOp (withTransactionModeRetryImpl (mapHandle psql) mode shouldRetry f)

-- | Lifted 'PSQL.withTransactionMode'.
withTransactionModeRetry' ::
  (HasCallStack, e :> es, E.Exception exc) =>
  PostgreSQL e ->
  PSQL.TransactionMode ->
  (exc -> Bool) ->
  Eff es a ->
  Eff es a
withTransactionModeRetry' psql mode shouldRetry f = makeOp (withTransactionModeRetry'Impl (mapHandle psql) mode shouldRetry f)

-- | Lifted 'PSQL.withTransaction'.
withTransactionSerializable ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  Eff es a ->
  Eff es a
withTransactionSerializable psql f = makeOp (withTransactionSerializableImpl (mapHandle psql) f)

-- | British alias of 'withTransactionSerializable'.
withTransactionSerialisable ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  Eff es a ->
  Eff es a
withTransactionSerialisable = withTransactionSerializable
{-# INLINE withTransactionSerialisable #-}

-- | Lifted 'PSQL.withSavepoint'.
withSavepoint ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  Eff es a ->
  Eff es a
withSavepoint psql f = makeOp (withSavepointImpl (mapHandle psql) f)

-- | Lifted 'PSQL.begin'.
begin ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  Eff es ()
begin psql = makeOp (beginImpl (mapHandle psql))

-- | Lifted 'PSQL.commit'.
commit ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  Eff es ()
commit psql = makeOp (commitImpl (mapHandle psql))

-- | Lifted 'PSQL.rollback'.
rollback ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  Eff es ()
rollback psql = makeOp (rollbackImpl (mapHandle psql))

-- | Lifted 'PSQL.fold'.
fold ::
  (HasCallStack, e :> es, PSQL.FromRow row, PSQL.ToRow params) =>
  PostgreSQL e ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
fold psql q params a f = makeOp (foldImpl (mapHandle psql) q params a f)

-- | Lifted 'PSQL.foldWithOptions'.
foldWithOptions ::
  (HasCallStack, e :> es, PSQL.FromRow row, PSQL.ToRow params) =>
  PostgreSQL e ->
  PSQL.FoldOptions ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptions psql opts q params a f = makeOp (foldWithOptionsImpl (mapHandle psql) opts q params a f)

-- | Lifted 'PSQL.fold_'.
fold_ ::
  (HasCallStack, e :> es, PSQL.FromRow row) =>
  PostgreSQL e ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
fold_ psql q a f = makeOp (fold_Impl (mapHandle psql) q a f)

-- | Lifted 'PSQL.foldWithOptions_'.
foldWithOptions_ ::
  (HasCallStack, e :> es, PSQL.FromRow row) =>
  PostgreSQL e ->
  PSQL.FoldOptions ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptions_ psql opts q a f = makeOp (foldWithOptions_Impl (mapHandle psql) opts q a f)

-- | Lifted 'PSQL.forEach'.
forEach ::
  (HasCallStack, e :> es, PSQL.FromRow r, PSQL.ToRow q) =>
  PostgreSQL e ->
  PSQL.Query ->
  q ->
  (r -> Eff es ()) ->
  Eff es ()
forEach psql q row forR = makeOp (forEachImpl (mapHandle psql) q row forR)

-- | Lifted 'PSQL.forEach_'.
forEach_ ::
  (HasCallStack, e :> es, PSQL.FromRow r) =>
  PostgreSQL e ->
  PSQL.Query ->
  (r -> Eff es ()) ->
  Eff es ()
forEach_ psql q forR = makeOp (forEach_Impl (mapHandle psql) q forR)

-- | Lifted 'PSQL.returning'.
returning ::
  (HasCallStack, e :> es, PSQL.ToRow q, PSQL.FromRow r) =>
  PostgreSQL e ->
  PSQL.Query ->
  [q] ->
  Eff es [r]
returning psql q rows = makeOp (returningImpl (mapHandle psql) q rows)

-- | Lifted 'PSQL.foldWith'.
foldWith ::
  (HasCallStack, e :> es, PSQL.ToRow params) =>
  PostgreSQL e ->
  PSQL.RowParser row ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWith psql parser q params a f = makeOp (foldWithImpl (mapHandle psql) parser q params a f)

-- | Lifted 'PSQL.foldWithOptionsAndParser'.
foldWithOptionsAndParser ::
  (HasCallStack, e :> es, PSQL.ToRow params) =>
  PostgreSQL e ->
  PSQL.FoldOptions ->
  PSQL.RowParser row ->
  PSQL.Query ->
  params ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptionsAndParser psql opts parser q params a f = makeOp (foldWithOptionsAndParserImpl (mapHandle psql) opts parser q params a f)

-- | Lifted 'PSQL.foldWith_'.
foldWith_ ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  PSQL.RowParser row ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWith_ psql parser q a f = makeOp (foldWith_Impl (mapHandle psql) parser q a f)

-- | Lifted 'PSQL.foldWithOptionsAndParser_'.
foldWithOptionsAndParser_ ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  PSQL.FoldOptions ->
  PSQL.RowParser row ->
  PSQL.Query ->
  a ->
  (a -> row -> Eff es a) ->
  Eff es a
foldWithOptionsAndParser_ psql opts parser q a f = makeOp (foldWithOptionsAndParser_Impl (mapHandle psql) opts parser q a f)

-- | Lifted 'PSQL.forEachWith'.
forEachWith ::
  (HasCallStack, e :> es, PSQL.ToRow q) =>
  PostgreSQL e ->
  PSQL.RowParser r ->
  PSQL.Query ->
  q ->
  (r -> Eff es ()) ->
  Eff es ()
forEachWith psql parser q row forR = makeOp (forEachWithImpl (mapHandle psql) parser q row forR)

-- | Lifted 'PSQL.forEachWith_'.
forEachWith_ ::
  (HasCallStack, e :> es) =>
  PostgreSQL e ->
  PSQL.RowParser r ->
  PSQL.Query ->
  (r -> Eff es ()) ->
  Eff es ()
forEachWith_ psql parser row forR = makeOp (forEachWith_Impl (mapHandle psql) parser row forR)

-- | Lifted 'PSQL.returningWith'.
returningWith ::
  (HasCallStack, e :> es, PSQL.ToRow q) =>
  PostgreSQL e ->
  PSQL.RowParser r ->
  PSQL.Query ->
  [q] ->
  Eff es [r]
returningWith psql parser q rows = makeOp (returningWithImpl (mapHandle psql) parser q rows)

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

(...) :: (a -> b) -> (t1 -> t2 -> a) -> t1 -> t2 -> b
unlift ... f = \a' row -> unlift $ f a' row

{- | Obvious interepreter for 'PostgreSQL'. Just gets a 'PSQL.Connection' from 'WithConnection' and calls the
corresponding function from "Database.PostgreSQL.Simple".
-}
runPostgreSQL ::
  forall e1 e2 es b.
  (HasCallStack, e1 :> es, e2 :> es) =>
  WithConnection e1 ->
  IOE e2 ->
  (forall e. PostgreSQL e -> Eff (e :& es) b) ->
  Eff es b
runPostgreSQL withConn ioe k =
  useImplIn k MkPostgreSQL {..}
  where
    queryImpl :: forall q r. (PSQL.ToRow q, PSQL.FromRow r) => PSQL.Query -> q -> Eff es [r]
    queryImpl q row = withConnection withConn $ \conn -> effIO ioe (PSQL.query conn q row)

    queryWithImpl :: forall q r. (PSQL.ToRow q) => PSQL.RowParser r -> PSQL.Query -> q -> Eff es [r]
    queryWithImpl parser q row =
      withConnection withConn $ \conn -> effIO ioe (PSQL.queryWith parser conn q row)

    query_Impl :: forall r. (PSQL.FromRow r) => PSQL.Query -> Eff es [r]
    query_Impl row =
      withConnection withConn $ \conn -> effIO ioe (PSQL.query_ conn row)

    queryWith_Impl :: forall r. PSQL.RowParser r -> PSQL.Query -> Eff es [r]
    queryWith_Impl parser row =
      withConnection withConn $ \conn -> effIO ioe (PSQL.queryWith_ parser conn row)

    executeImpl :: forall q. (PSQL.ToRow q) => PSQL.Query -> q -> Eff es Int64
    executeImpl q row = withConnection withConn $ \conn -> effIO ioe (PSQL.execute conn q row)

    execute_Impl :: PSQL.Query -> Eff es Int64
    execute_Impl q = withConnection withConn $ \conn -> effIO ioe (PSQL.execute_ conn q)

    executeManyImpl :: forall q. (PSQL.ToRow q) => PSQL.Query -> [q] -> Eff es Int64
    executeManyImpl q row = withConnection withConn $ \conn -> effIO ioe (PSQL.executeMany conn q row)

    withTransactionImpl :: forall e' a. Eff e' a -> Eff (e' :& es) a
    withTransactionImpl f = unliftWithConn withConn ioe $ \conn unlift -> PSQL.withTransaction conn (unlift $ useImpl f)

    withTransactionLevelImpl :: forall e' a. PSQL.IsolationLevel -> Eff e' a -> Eff (e' :& es) a
    withTransactionLevelImpl level f = unliftWithConn withConn ioe $ \conn unlift -> PSQL.withTransactionLevel level conn (unlift $ useImpl f)

    withTransactionModeImpl :: forall e' a. PSQL.TransactionMode -> Eff e' a -> Eff (e' :& es) a
    withTransactionModeImpl mode f = unliftWithConn withConn ioe $ \conn unlift -> PSQL.withTransactionMode mode conn (unlift $ useImpl f)

    withTransactionModeRetryImpl :: forall e' a. PSQL.TransactionMode -> (PSQL.SqlError -> Bool) -> Eff e' a -> Eff (e' :& es) a
    withTransactionModeRetryImpl mode shouldRetry f = unliftWithConn withConn ioe $ \conn unlift -> PSQL.withTransactionModeRetry mode shouldRetry conn (unlift $ useImpl f)

    withTransactionModeRetry'Impl :: forall exc e' a. (E.Exception exc) => PSQL.TransactionMode -> (exc -> Bool) -> Eff e' a -> Eff (e' :& es) a
    withTransactionModeRetry'Impl mode shouldRetry f = unliftWithConn withConn ioe $ \conn unlift -> PSQL.withTransactionModeRetry' mode shouldRetry conn (unlift $ useImpl f)

    withTransactionSerializableImpl :: forall e' a. Eff e' a -> Eff (e' :& es) a
    withTransactionSerializableImpl f = unliftWithConn withConn ioe $ \conn unlift -> PSQL.withTransactionSerializable conn (unlift $ useImpl f)

    withSavepointImpl :: forall e' a. Eff e' a -> Eff (e' :& es) a
    withSavepointImpl f = unliftWithConn withConn ioe $ \conn unlift -> PSQL.withSavepoint conn (unlift $ useImpl f)

    beginImpl :: Eff es ()
    beginImpl = withConnection withConn $ effIO ioe . PSQL.begin

    commitImpl :: Eff es ()
    commitImpl = withConnection withConn $ effIO ioe . PSQL.commit

    rollbackImpl :: Eff es ()
    rollbackImpl = withConnection withConn $ effIO ioe . PSQL.rollback

    foldImpl ::
      forall row params e' a.
      (PSQL.FromRow row, PSQL.ToRow params) =>
      PSQL.Query ->
      params ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& es) a
    foldImpl q params a f =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.fold conn q params a (unlift ... (useImpl ... f))

    fold_Impl ::
      forall row e' a.
      (PSQL.FromRow row) =>
      PSQL.Query ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& es) a
    fold_Impl q a f =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.fold_ conn q a (unlift ... (useImpl ... f))

    foldWithOptionsImpl ::
      forall row params e' a.
      (PSQL.FromRow row, PSQL.ToRow params) =>
      PSQL.FoldOptions ->
      PSQL.Query ->
      params ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& es) a
    foldWithOptionsImpl opts q params a f =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.foldWithOptions opts conn q params a (unlift ... (useImpl ... f))

    foldWithOptions_Impl ::
      forall row e' a.
      (PSQL.FromRow row) =>
      PSQL.FoldOptions ->
      PSQL.Query ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& es) a
    foldWithOptions_Impl opts q a f =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.foldWithOptions_ opts conn q a (unlift ... (useImpl ... f))

    forEachImpl ::
      forall r q e'.
      (PSQL.FromRow r, PSQL.ToRow q) =>
      PSQL.Query ->
      q ->
      (r -> Eff e' ()) ->
      Eff (e' :& es) ()
    forEachImpl q row forR =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.forEach conn q row (unlift . useImpl . forR)

    forEach_Impl ::
      forall r e'.
      (PSQL.FromRow r) =>
      PSQL.Query ->
      (r -> Eff e' ()) ->
      Eff (e' :& es) ()
    forEach_Impl q forR =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.forEach_ conn q (unlift . useImpl . forR)

    returningImpl ::
      forall r q.
      (PSQL.ToRow q, PSQL.FromRow r) =>
      PSQL.Query ->
      [q] ->
      Eff es [r]
    returningImpl q rows = withConnection withConn $ \conn -> effIO ioe $ PSQL.returning conn q rows

    foldWithImpl ::
      forall row params e' a.
      (PSQL.ToRow params) =>
      PSQL.RowParser row ->
      PSQL.Query ->
      params ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& es) a
    foldWithImpl parser q params a f =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.foldWith parser conn q params a (unlift ... (useImpl ... f))

    foldWithOptionsAndParserImpl ::
      forall row params e' a.
      (PSQL.ToRow params) =>
      PSQL.FoldOptions ->
      PSQL.RowParser row ->
      PSQL.Query ->
      params ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& es) a
    foldWithOptionsAndParserImpl opts parser q params a f =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.foldWithOptionsAndParser opts parser conn q params a (unlift ... (useImpl ... f))

    foldWith_Impl ::
      forall row e' a.
      PSQL.RowParser row ->
      PSQL.Query ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& es) a
    foldWith_Impl parser q a f =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.foldWith_ parser conn q a (unlift ... (useImpl ... f))

    foldWithOptionsAndParser_Impl ::
      forall row e' a.
      PSQL.FoldOptions ->
      PSQL.RowParser row ->
      PSQL.Query ->
      a ->
      (a -> row -> Eff e' a) ->
      Eff (e' :& es) a
    foldWithOptionsAndParser_Impl opts parser q a f =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.foldWithOptionsAndParser_ opts parser conn q a (unlift ... (useImpl ... f))

    forEachWithImpl ::
      forall r q e'.
      (PSQL.ToRow q) =>
      PSQL.RowParser r ->
      PSQL.Query ->
      q ->
      (r -> Eff e' ()) ->
      Eff (e' :& es) ()
    forEachWithImpl parser q row forR =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.forEachWith parser conn q row (unlift . useImpl . forR)

    forEachWith_Impl ::
      forall r e'.
      PSQL.RowParser r ->
      PSQL.Query ->
      (r -> Eff e' ()) ->
      Eff (e' :& es) ()
    forEachWith_Impl parser q forR =
      unliftWithConn withConn ioe $ \conn unlift ->
        PSQL.forEachWith_ parser conn q (unlift . useImpl . forR)

    returningWithImpl ::
      forall r q.
      (PSQL.ToRow q) =>
      PSQL.RowParser r ->
      PSQL.Query ->
      [q] ->
      Eff es [r]
    returningWithImpl parser q rows = withConnection withConn $ \conn -> effIO ioe $ PSQL.returningWith parser conn q rows
