module Bluefin.Opaleye
  ( -- * Effect
    Opaleye (..)

    -- * Effectful functions

    -- ** Select
  , runSelect
  , runSelectI
  , runSelectExplicit

    -- ** Select-fold
  , runSelectFold
  , runSelectFoldExplicit

    -- ** Insert
  , runInsert

    -- ** Delete
  , runDelete

    -- ** Update
  , runUpdate

    -- * Interpreters
  , runOpaleyeWithConnection
  -- , runOpaleyeWithConnectionCounting
  , runOpaleyeConnection
  -- , runOpaleyeConnectionCounting

    -- * Counting SQL operations
  , SQLOperationCounts (..)
  , withCounts
  , printCounts
  )
where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Opaleye.Count
import Bluefin.Opaleye.Effect
import qualified Bluefin.PostgreSQL.Connection as Conn
import Data.Profunctor.Product.Default
import qualified Database.PostgreSQL.Simple as PSQL
import GHC.Stack
import qualified Opaleye as O
import qualified Opaleye.Internal.Inferrable as O

-- | Lifted 'O.runSelect'.
runSelect ::
  (HasCallStack, e :> es, Default O.FromFields fields haskells) =>
  Opaleye e ->
  O.Select fields ->
  Eff es [haskells]
runSelect o = runSelectExplicit o def

-- | Lifted 'O.runSelectFold'.
runSelectFold ::
  (HasCallStack, e :> es, Default O.FromFields fields haskells) =>
  Opaleye e ->
  O.Select fields ->
  b ->
  (b -> haskells -> Eff es b) ->
  Eff es b
runSelectFold o = runSelectFoldExplicit o def

-- | Lifted 'O.runSelectI'.
runSelectI ::
  (HasCallStack, e :> es, Default (O.Inferrable O.FromFields) fields haskells) =>
  Opaleye e ->
  O.Select fields ->
  Eff es [haskells]
runSelectI o = runSelectExplicit o (O.runInferrable def)

{- | Interpret the 'Opaleye' effect using 'Conn.WithConnection' from
[bluefin-postgresql](https://hackage.haskell.org/package/bluefin-postgresql).

If you don't want to use 'Conn.WithConnection', see 'runOpaleyeConnection'.
-}
runOpaleyeWithConnection ::
  (HasCallStack, e1 :> es, e2 :> es) =>
  Conn.WithConnection e1 ->
  IOE e2 ->
  (forall e. Opaleye e -> Eff (e :& es) a) ->
  Eff es a
runOpaleyeWithConnection wc ioe k =
  useImplIn
    k
    MkOpaleye
      { runSelectExplicitImpl = \ff sel ->
          Conn.withConnection wc $ \conn -> effIO ioe $ O.runSelectExplicit ff conn sel
      , runSelectFoldExplicitImpl = \ff sel initial foldFn ->
          Conn.withConnection wc $ \conn ->
            withEffToIO_ ioe $ \unlift ->
              O.runSelectFoldExplicit ff conn sel initial $ \acc new ->
                unlift $ useImpl $ foldFn acc new
      , runInsertImpl = \sel ->
          Conn.withConnection wc $ \conn -> effIO ioe $ O.runInsert conn sel
      , runDeleteImpl = \sel ->
          Conn.withConnection wc $ \conn -> effIO ioe $ O.runDelete conn sel
      , runUpdateImpl = \sel ->
          Conn.withConnection wc $ \conn -> effIO ioe $ O.runUpdate conn sel
      }

-- | Interpret the 'Opaleye' effect by simply supplying a 'PSQL.Connection'
runOpaleyeConnection ::
  (HasCallStack, e1 :> es) =>
  IOE e1 ->
  PSQL.Connection ->
  (forall e. Opaleye e -> Eff (e :& es) a) ->
  Eff es a
runOpaleyeConnection ioe conn k =
  useImplIn
    k
    MkOpaleye
      { runSelectExplicitImpl = \ff sel -> effIO ioe $ O.runSelectExplicit ff conn sel
      , runSelectFoldExplicitImpl = \ff sel initial foldFn ->
          withEffToIO_ ioe $ \unlift ->
            O.runSelectFoldExplicit ff conn sel initial $
              \acc new -> unlift $ useImpl $ foldFn acc new
      , runInsertImpl = effIO ioe . O.runInsert conn
      , runDeleteImpl = effIO ioe . O.runDelete conn
      , runUpdateImpl = effIO ioe . O.runUpdate conn
      }

{- | Same as 'runOpaleyeWithConnection', but we track the number of SQL operations that
we perform.
runOpaleyeWithConnectionCounting ::
  forall a es.
  (HasCallStack, State SQLOperationCounts :> es, Conn.WithConnection :> es, IOE :> es) =>
  Eff (Opaleye : es) a ->
  Eff es a
runOpaleyeWithConnectionCounting = runOpaleyeWithConnection . opaleyeAddCounting

{\- | Same as 'runOpaleyeConnection', but we track the number of SQL operations that
we perform.
-\}
runOpaleyeConnectionCounting ::
  (HasCallStack, State SQLOperationCounts :> es, IOE :> es) =>
  PSQL.Connection ->
  Eff (Opaleye : es) a ->
  Eff es a
runOpaleyeConnectionCounting conn = runOpaleyeConnection conn . opaleyeAddCounting
-}
