# bluefin-postgresql

This package provides an `bluefin` effect for [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)'s `Connection` type.

It defines a dynamic effect to allow effectful functions to use a `Connection`, without worrying about where that `Connection` comes from.

For a higher-level effect library using [Opaleye](https://hackage.haskell.org/package/opaleye), see [bluefin-opaleye](https://github.com/fpringle/bluefin-postgresql/blob/main/bluefin-opaleye#readme).

## Effectful functions

In the `WithConnection` effect we can always request a `Connection` and use it as we normally
would:

```haskell
import Bluefin.PostgreSQL as BP
import qualified Database.PostgreSQL.Simple as PSQL

insertAndList ::
  (e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  Eff es [User]
insertAndList wc ioe = BP.withConnection wc $ \conn -> do
  effIO ioe $ PSQL.execute conn "insert into users (first_name) values (?)" ["Nuala"]
  effIO ioe $ PSQL.query conn "select * from users where first_name in ?" $ PSQL.Only $ PSQL.In ["Anna", "Boris", "Carla"]
```

In fact, for convenience we also define lifted versions of all of the query/execute
functions from `postgresql-simple`, so we can completely forget about `Connection` and rewrite the above to:

```haskell

import Bluefin.PostgreSQL

insertAndList ::
  (e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  Eff es [User]
insertAndList wc ioe = do
  BP.execute wc ioe "insert into users (first_name) values (?)" ["Nuala"]
  BP.query wc ioe "select * from users where first_name in ?" $ PSQL.Only $ PSQL.In ["Anna", "Boris", "Carla"]
```

The same goes for other functions:

```haskell
-- use a transaction
insertAndListCarefully ::
  (e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  Eff es [User]
insertAndListCarefully wc ioe = BP.withTransaction wc ioe $ insertAndList wc ioe

-- stream + fold over results (in Eff)
countUsersIneffeciently ::
  (e :> es, e1 :> es) =>
  WithConnection e ->
  IOE e1 ->
  Eff es Int
countUsersIneffeciently wc ioe =
  BP.fold_ wc ioe "select * from users" 0 $ \acc (row :: User) -> do
    effIO ioe . putStrLn $ "User: " <> show row
    pure $ acc + 1
```

## Interpreters

The simplest way of running the `WithConnection` effect is by just providing a `Connection`, which we can get in the normal ways:

```haskell
import Bluefin.PostgreSQL as BP
import qualified Database.PostgreSQL.Simple as PSQL

usingConnection :: IO ()
usingConnection =
  runEff $ \ioe ->
    bracket (effIO ioe $ PSQL.connectPostgreSQL "") (effIO ioe . PSQL.close) $ \conn ->
      BP.runWithConnection conn $ \wc -> insertAndListCarefully wc ioe >>= effIO ioe . print

usingConnectInfo :: IO ()
usingConnectInfo =
  runEff $ \ioe ->
    BP.runWithConnectInfo ioe PSQL.defaultConnectInfo $ \wc ->
      insertAndListCarefully wc ioe >>= effIO ioe . print
```

Alternatively, we can use a connection pool (from [resource-pool](https://hackage.haskell.org/package/resource-pool)
and [unliftio-pool](https://hackage.haskell.org/package/unliftio-pool)), which is much better suited to
long-running processes like servers.

```haskell
import Bluefin.PostgreSQL as BP
import qualified Database.PostgreSQL.Simple as PSQL
import qualified UnliftIO.Pool as P

usingConnectionPool :: IO ()
usingConnectionPool = do
  poolCfg <- P.mkDefaultPoolConfig (PSQL.connectPostgreSQL "") PSQL.close 5.0 10
  pool <- P.newPool poolCfg
  runEff $ \ioe ->
    BP.runWithConnectionPool ioe pool $ \wc ->
      insertAndListCarefully wc ioe >>= effIO ioe . print
```
