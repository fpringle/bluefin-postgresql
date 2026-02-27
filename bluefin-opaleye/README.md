# bluefin-opaleye

This package provides a `bluefin` effect for [Opaleye](https://hackage.haskell.org/package/opaleye) operations.

It combines the very safe, high-level syntax of Opaleye, with the `WithConnection` abstraction of [bluefin-postgresql](https://github.com/fpringle/bluefin-postgresql/blob/main/bluefin-postgresql#readme).

## Effectful functions

In the `Opaleye` effect we can perform the 4 main operations permitted by Opaleye: query, insert, delete, and update.

```haskell
{-# LANGUAGE Arrows #-}
import Control.Arrow
import Bluefin.Opaleye as BO
import qualified Opaleye as O

insertAndList :: (e :> es) => Opaleye e -> Eff es [User]
insertAndList o = do
  BO.runInsert o $ O.Insert userTable [User {firstName = "Nuala"}] O.rCount Nothing

  BO.runDelete o $ O.Delete userTable isAdmin O.rCount

  BO.runUpdate o $ O.Update userTable (\user -> user {updatedAt = O.now}) isAdmin O.rCount

  BO.runSelect o $ proc () -> do
    user <- O.selectTable userTable -< ()
    O.restrict -< firstName user `O.in_` (O.toFields <$> ["Anna", "Boris", "Carla"])
    returnA -< user
```

## Interpreters

To run the `Opaleye` effect we can use the `WithConnection` effect from [bluefin-postgresql](https://github.com/fpringle/bluefin-postgresql/blob/main/bluefin-postgresql#readme):

```haskell
import Bluefin.Opaleye as BO

doOpaleyeStuff :: (e :> es) => WithConnection e -> IOE e -> Eff es [User]
doOpaleyeStuff wc ioe =
  BO.runOpaleyeWithConnection wc ioe $ \o -> insertAndList o
```

The `WithConnection` effect can then be dispatched using one of its [interpreters](https://github.com/fpringle/bluefin-postgresql/blob/main/bluefin-postgresql#interpreters).
Or, to skip that entirely, we can just use `runOpaleyeConnection`:

```haskell
doOpaleyeStuff :: (e :> es) => IOE e -> PSQL.Connection -> Eff es [User]
doOpaleyeStuff ioe conn = BO.runOpaleyeConnection ioe conn insertAndList
```
