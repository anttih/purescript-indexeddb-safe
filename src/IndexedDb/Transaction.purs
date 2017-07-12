module IndexedDb.Transaction
  ( Transaction
  , TransactionF
  , VersionMigration(..)
  , VersionChangeTx
  , Read
  , Write
  , VersionChange
  , add
  , createObjectStore
  , delete
  , get
  , index
  , open
  , put
  , runReadTx
  , runReadWriteTx
  ) where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import DOM.Exception (DOMException)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as JA
import Data.Either (Either(Right, Left))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList, head)
import Data.Maybe (Maybe(..))
import Data.Record (class LabelsToList, RLProxy(..), labelsToList)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Traversable (traverse)
import IndexedDb.Key (class IsKey, Key, toKey)
import IndexedDb.Request (Request)
import IndexedDb.Request as Req
import IndexedDb.Store (Store(..))
import IndexedDb.Types (Database, IDB, IDBDatabase, IDBObjectStore, IDBTransaction, KeyPath(KeyPath), StoreName(StoreName), TxMode(TxMode), Version)
import Type.Row as R

-- | IDBTransactionMode value "read"
data Read

-- | Transaction mode corresponding to "readwrite" IDBTransactionMode.
data Write

-- | IDBTransactionMode value "versionchange"
data VersionChange

data TransactionF (r ∷ # Type) a
  = Add StoreName Json a
  | Get StoreName Key (Maybe Json → Either JsonDecodeError a)
  | Delete StoreName Key a
  | Put StoreName Json a
  | CreateObjectStore String KeyPath (List.List String) (IDBObjectStore → a)
  | Index StoreName String Key (Maybe Json → Either JsonDecodeError a)

type Transaction r a = Free (TransactionF r) a

runTx
  :: forall eff r k ir a b
  . TxMode
  → IDBDatabase
  → Array (Store k ir b)
  → Transaction r a
  → Request (idb :: IDB | eff) a
runTx mode idb stores tx = do
  itx ← Req.transaction idb (storeName <$> stores) mode
  foldFree (evalTx idb itx) tx

  where
  storeName ∷ Store k ir b → StoreName
  storeName (Store { name }) = (StoreName name)

-- | Run a transaction in `read` mode
runReadTx
  ∷ forall eff k ir a b
  . IDBDatabase
  → Array (Store k ir b)
  → Transaction (read ∷ Read) a
  → Request (idb :: IDB | eff) a
runReadTx = runTx (TxMode "read")

-- | Run a transaction in `readwrite` mode
runReadWriteTx
  ∷ forall eff k ir a b
  . IDBDatabase
  → Array (Store k ir b)
  → Transaction (read ∷ Read, write ∷ Write) a
  → Request (idb :: IDB | eff) a
runReadWriteTx = runTx (TxMode "readwrite")

type VersionChangeTx a
  = Transaction (read ∷ Read, write ∷ Write, versionchange ∷ VersionChange) a

data VersionMigration = VersionMigration Version (VersionChangeTx Unit)

migration ∷ VersionMigration → VersionChangeTx Unit
migration (VersionMigration _ tx) = tx

-- | Opens the given database and migrates to the given version
open
  ∷ ∀ eff
  . Database
  → Version
  → NonEmptyList VersionMigration
  → Request (idb ∷ IDB | eff) IDBDatabase
open db version migrations = Req.open db version \versionChange idb itx → void do
  let req = foldFree (evalTx idb itx) (migrate version version migrations)
  runAff (const $ Req.abort itx) (handle itx) (runExceptT req)

  where
  -- TODO: Actually migrate
  migrate ∷ Version → Version → NonEmptyList VersionMigration → VersionChangeTx Unit
  migrate _ _ migrations' = migration $ head migrations'

  handle ∷ ∀ a. IDBTransaction → Either DOMException a → Eff (idb ∷ IDB | eff) Unit
  handle tx' = case _ of
    -- abort the transaction, this will call the error handler on the IndexedDB open request
    Left _ → Req.abort tx'
    -- All ok, do nothing. The open request onsuccess callback will be called.
    Right _ → pure unit

-- | Adds a record to a store.
add ∷ ∀ e k ir a. Store k ir a → a → Transaction (write ∷ Write | e) Unit
add (Store { name, codec }) item = liftF $ Add (StoreName name) (JA.encode codec item) unit

-- | Updates an existing record or adds a new record with a given key.
put ∷ ∀ e k ir a. Store k ir a → a → Transaction (write ∷ Write | e) Unit
put (Store { name, codec }) item = liftF
  $ Put (StoreName name) (JA.encode codec item) unit

-- | Gets a record by the primary key.
get ∷ ∀ e k ir a. IsKey k ⇒ Store k ir a → k → Transaction (read ∷ Read | e) (Maybe a)
get (Store { name, codec }) key = liftF $ Get (StoreName name) (toKey key) dec
  where
  dec ∷ Maybe Json → Either JsonDecodeError (Maybe a)
  dec = case _ of
          Nothing → pure Nothing
          Just json → pure <$> JA.decode codec json

-- | Deletes a record by the primary key.
delete ∷ ∀ e k ir a. IsKey k ⇒ Store k ir a → k → Transaction (write ∷ Write | e) Unit
delete (Store { name }) key = liftF $ Delete (StoreName name) (toKey key) unit

index
  ∷ ∀ e k l t r r2 a it ir
  . IsKey t
  ⇒ IsSymbol l
  ⇒ RowCons l t r a -- get the type of t
  ⇒ RowCons l it r2 ir -- check that l is a label in ir
  ⇒ Store k (Record ir) (Record a) → SProxy l → t → Transaction (read ∷ Read | e) (Maybe (Record a))
index (Store { name, codec }) key v = liftF
  $ Index (StoreName name) (reflectSymbol key) (toKey v) dec

  where
  dec ∷ Maybe Json → Either JsonDecodeError (Maybe (Record a))
  dec = case _ of
          Nothing → pure Nothing
          Just json → pure <$> JA.decode codec json

-- | Create an object store.
createObjectStore
  ∷ ∀ e k ir rl a
  . R.RowToList ir rl
  ⇒ LabelsToList rl
  ⇒ Store k (Record ir) a
  → Transaction (versionchange ∷ VersionChange | e) IDBObjectStore
createObjectStore (Store { name, keyPath }) = liftF
  $ CreateObjectStore name (KeyPath keyPath) (labelsToList (RLProxy ∷ RLProxy rl)) id

-- The implementation here ensures that all requests are run within
-- the same transaction.
evalTx :: ∀ r eff. IDBDatabase → IDBTransaction → (TransactionF r) ~> Request (idb :: IDB | eff)
evalTx idb tx = case _ of
  Add storeName d next → do
    store ← Req.objectStore storeName tx
    res ← Req.add store d
    pure next
  Get storeName key f → do
    store ← Req.objectStore storeName tx
    res ← Req.get store key
    case f res of
      Left e → liftAff $ throwError (error (printJsonDecodeError e))
      Right a → pure a
  Put storeName d next → do
    store ← Req.objectStore storeName tx
    Req.put store d
    pure next
  CreateObjectStore storeName keyPath indices f → do
    store ← Req.createObjectStore idb storeName keyPath
    _ ← traverse (\n → Req.createIndex store n (KeyPath n) true) indices
    pure (f store)
  Delete storeName key next → do
    store ← Req.objectStore storeName tx
    Req.delete store key
    pure next
  Index storeName key v f → do
    store ← Req.objectStore storeName tx
    res ← Req.index store key v
    case f res of
      Left e → liftAff $ throwError (error (printJsonDecodeError e))
      Right a → pure a

