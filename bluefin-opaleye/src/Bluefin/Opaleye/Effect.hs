module Bluefin.Opaleye.Effect
  ( -- * Effect
    Opaleye (..)

    -- ** Effectful functions
  , runSelectExplicit
  , runSelectFoldExplicit
  , runInsert
  , runDelete
  , runUpdate
  )
where

import Bluefin.Compound
import Bluefin.Eff
import qualified Opaleye as O

-- | A dynamic effect to perform @opaleye@ operations.
data Opaleye (e :: Effects) = MkOpaleye
  { runSelectExplicitImpl ::
      forall e' fields haskells.
      O.FromFields fields haskells ->
      O.Select fields ->
      Eff (e' :& e) [haskells]
  -- ^ Lifted 'O.RunSelectExplicit'.
  , runSelectFoldExplicitImpl ::
      forall e' fields haskells b.
      O.FromFields fields haskells ->
      O.Select fields ->
      b ->
      (b -> haskells -> Eff e' b) ->
      Eff (e' :& e) b
  -- ^ Lifted 'O.RunSelectFoldExplicit'.
  , runInsertImpl :: forall e' haskells. O.Insert haskells -> Eff (e' :& e) haskells
  -- ^ Lifted 'O.RunInsert'.
  , runDeleteImpl :: forall e' haskells. O.Delete haskells -> Eff (e' :& e) haskells
  -- ^ Lifted 'O.RunDelete'.
  , runUpdateImpl :: forall e' haskells. O.Update haskells -> Eff (e' :& e) haskells
  -- ^ Lifted 'O.RunUpdate'.
  }

instance Handle Opaleye where
  mapHandle h =
    MkOpaleye
      { runSelectExplicitImpl = \ff sel -> useImplUnder (runSelectExplicitImpl h ff sel)
      , runSelectFoldExplicitImpl = \ff sel b f -> useImplUnder (runSelectFoldExplicitImpl h ff sel b f)
      , runInsertImpl = useImplUnder . runInsertImpl h
      , runDeleteImpl = useImplUnder . runDeleteImpl h
      , runUpdateImpl = useImplUnder . runUpdateImpl h
      }

-- | Lifted 'O.RunSelectExplicit'.
runSelectExplicit ::
  (e :> es) =>
  Opaleye e ->
  O.FromFields fields haskells ->
  O.Select fields ->
  Eff es [haskells]
runSelectExplicit o ff sel = makeOp (runSelectExplicitImpl (mapHandle o) ff sel)

-- | Lifted 'O.RunSelectFoldExplicit'.
runSelectFoldExplicit ::
  (e :> es) =>
  Opaleye e ->
  O.FromFields fields haskells ->
  O.Select fields ->
  b ->
  (b -> haskells -> Eff es b) ->
  Eff es b
runSelectFoldExplicit o ff sel b f = makeOp (runSelectFoldExplicitImpl (mapHandle o) ff sel b f)

-- | Lifted 'O.RunInsert'.
runInsert ::
  (e :> es) =>
  Opaleye e ->
  O.Insert haskells ->
  Eff es haskells
runInsert o ins = makeOp (runInsertImpl (mapHandle o) ins)

-- | Lifted 'O.RunDelete'.
runDelete ::
  (e :> es) =>
  Opaleye e ->
  O.Delete haskells ->
  Eff es haskells
runDelete o ins = makeOp (runDeleteImpl (mapHandle o) ins)

-- | Lifted 'O.RunUpdate'.
runUpdate ::
  (e :> es) =>
  Opaleye e ->
  O.Update haskells ->
  Eff es haskells
runUpdate o ins = makeOp (runUpdateImpl (mapHandle o) ins)
