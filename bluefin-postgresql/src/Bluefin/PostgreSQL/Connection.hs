module Bluefin.PostgreSQL.Connection
  ( -- * Effect
    WithConnection (..)
  , withConnection

    -- * Interpret with a single Connection
  , runWithConnection
  , runWithConnectInfo
  )
where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import qualified Database.PostgreSQL.Simple as PSQL
import GHC.Stack

{- | A dynamic effect that lets us use a database 'PSQL.Connection',
without specifying __how__ that connection is supplied.

For example, we might want to just provide a connection and let the interpreter
use that for the whole procedure (see 'Bluefin.PostgreSQL.Connection.runWithConnection').

Or, we might want to create a connection pool which provides connections when
the interpreter asks for them (see "Bluefin.PostgreSQL.Connection.Pool").
-}
newtype WithConnection (e :: Effects) = MkWithConnection
  { withConnectionImpl :: forall e' a. (PSQL.Connection -> Eff e' a) -> Eff (e' :& e) a
  -- ^ Use a 'PSQL.Connection' provided by an interpreter.
  }

instance Handle WithConnection where
  mapHandle h =
    MkWithConnection
      { withConnectionImpl = useImplUnder . withConnectionImpl h
      }

-- | Use a 'PSQL.Connection' provided by an interpreter.
withConnection :: (e :> es) => WithConnection e -> (PSQL.Connection -> Eff es a) -> Eff es a
withConnection w f = makeOp (withConnectionImpl (mapHandle w) f)

{- | Run a t'WithConnection' effect by simply supplying a 'PSQL.Connection'.
The connection will be kept alive for the whole duration of the procedure,
which might not be want you want for long-running processes. If so, see
"Bluefin.PostgreSQL.Connection.Pool".
-}
runWithConnection ::
  PSQL.Connection ->
  (forall e. WithConnection e -> Eff (e :& es) a) ->
  Eff es a
runWithConnection conn k =
  useImplIn
    k
    MkWithConnection
      { withConnectionImpl = \f -> useImpl (f conn)
      }

{- | Run a t'WithConnection' effect using a 'PSQL.ConnectInfo'.
The 'PSQL.ConnectInfo' will be used to create a 'PSQL.Connection' which will be
kept alive for the whole duration of the procedure, which might not be want you want
for long-running processes. If so, see "Bluefin.PostgreSQL.Connection.Pool".

'PSQL.withConnect' will handle opening and closing the 'PSQL.Connection'.
-}
runWithConnectInfo ::
  (HasCallStack, e1 :> es) =>
  IOE e1 ->
  PSQL.ConnectInfo ->
  (forall e. WithConnection e -> Eff (e :& es) a) ->
  Eff es a
runWithConnectInfo ioe connInfo k =
  withEffToIO_ ioe $ \unlift ->
    PSQL.withConnect connInfo $ \conn ->
      unlift $ runWithConnection conn k
