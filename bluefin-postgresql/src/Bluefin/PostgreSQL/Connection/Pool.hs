module Bluefin.PostgreSQL.Connection.Pool
  ( -- * Interpret with a Connection pool
    runWithConnectionPool

    -- * Re-export
  , module Pool
  )
where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.PostgreSQL.Connection
import qualified Database.PostgreSQL.Simple as PSQL
import GHC.Stack
import UnliftIO.Pool as Pool

{- | Rather than keeping one connection alive and re-using it for the whole
process, we might want to create a 'Pool' of connections and only "ask" for
one when we need it. This function uses "UnliftIO.Pool" to do just that.
-}
runWithConnectionPool ::
  (HasCallStack, e1 :> es) =>
  IOE e1 ->
  Pool.Pool PSQL.Connection ->
  (forall e. WithConnection e -> Eff (e :& es) a) ->
  Eff es a
runWithConnectionPool ioe pool k =
  useImplIn
    k
    MkWithConnection
      { withConnectionImpl = \f ->
          runEffReader ioe $
            Pool.withResource pool $
              \conn -> effReader $
                \_ -> useImpl (f conn)
      }
