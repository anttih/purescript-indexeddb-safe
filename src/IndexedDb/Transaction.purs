module IndexedDb.Transaction
  ( Transaction
  , TransactionF
  , VersionMigration(..)
  , VersionChangeTx
  , Read
  , Write
  , VersionChange
  , class IndexQuery
  , add
  , clear
  , createObjectStore
  , createIndex
  , delete
  , get
  , getAll'
  , getAll
  , index
  , open
  , put
  , runReadTx
  , runReadWriteTx
  ) where

import Prelude

import Control.Monad.Aff (Error, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, runExceptT, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import DOM.Exception (DOMException)
import Data.Codec as Codec
import Data.Either (Either(Right, Left))
import Data.Exists as Exists
import Data.Foreign (F, Foreign)
import Data.List (take)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList, drop)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toMaybe)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Traversable (sequence, traverse)
import IndexedDb.Index (class RowListToIndices, NonUnique, Unique, IndexSpec, rowListToIndices)
import IndexedDb.Key (class IsKey, Key, toKey)
import IndexedDb.KeyRange (KeyRange)
import IndexedDb.Request (Request)
import IndexedDb.Request as Req
import IndexedDb.Store (Store(Store))
import IndexedDb.Types (Database, IDB, IDBDatabase, IDBIndex, IDBObjectStore, IDBTransaction, KeyPath(KeyPath), StoreName(StoreName), TxMode(TxMode), Version, VersionChangeEventInit(..))
import Type.Row as R

-- | IDBTransactionMode value "read"
data Read

-- | Transaction mode corresponding to "readwrite" IDBTransactionMode.
data Write

-- | IDBTransactionMode value "versionchange"
data VersionChange

data TransactionF (r :: # Type) a
  = Add StoreName Foreign a
  | Clear StoreName a
  | Get StoreName Key (Maybe Foreign -> F a)
  | GetAll StoreName (Maybe (Exists.Exists KeyRange)) (Array Foreign -> F a)
  | Delete StoreName Key a
  | Put StoreName Foreign a
  | CreateIndex StoreName String KeyPath Boolean (IDBIndex -> a)
  | DeleteIndex StoreName String a
  | CreateObjectStore String KeyPath (List.List IndexSpec) (IDBObjectStore -> a)
  | IndexUnique StoreName String Key (Maybe Foreign -> F a)
  | IndexNonUnique StoreName String Key (Array Foreign -> F a)

type Transaction r a = Free (TransactionF r) a

runTx
  :: forall eff r k ir a b
  . TxMode
  -> IDBDatabase
  -> Array (Store k ir b)
  -> Transaction r a
  -> Request (idb :: IDB | eff) a
runTx mode idb stores tx = do
  itx <- Req.transaction idb (storeName <$> stores) mode
  foldFree (evalTx idb itx) tx

  where
    storeName :: Store k ir b -> StoreName
    storeName (Store { name }) = (StoreName name)

-- | Run a transaction in `read` mode
runReadTx
  :: forall eff k ir a b
  . IDBDatabase
  -> Array (Store k ir b)
  -> Transaction (read :: Read) a
  -> Request (idb :: IDB | eff) a
runReadTx = runTx (TxMode "readonly")

-- | Run a transaction in `readwrite` mode
runReadWriteTx
  :: forall eff k ir a b
  . IDBDatabase
  -> Array (Store k ir b)
  -> Transaction (read :: Read, write :: Write) a
  -> Request (idb :: IDB | eff) a
runReadWriteTx = runTx (TxMode "readwrite")

type VersionChangeTx a
  = Transaction (read :: Read, write :: Write, versionchange :: VersionChange) a

newtype VersionMigration = VersionMigration (VersionChangeTx Unit)

migration :: VersionMigration -> VersionChangeTx Unit
migration (VersionMigration tx) = tx

-- | Opens the given database and migrates to the given version
open
  :: forall eff
  . Database
  -> Version
  -> NonEmptyList VersionMigration
  -> Request (idb :: IDB | eff) IDBDatabase
open db version migrations = Req.open db version \versionChange idb itx -> void do
  let req = foldFree (evalTx idb itx) (migrate versionChange migrations)
  runAff (handle itx) (runExceptT req)

  where
    -- Do nothing when there is no newVersion. This means that we are deleting the database.
    migrate :: VersionChangeEventInit -> NonEmptyList VersionMigration -> VersionChangeTx Unit
    migrate (VersionChangeEventInit {oldVersion, newVersion}) migrations' =
      maybe (pure unit) doMigrate (toMaybe newVersion)
      where
        doMigrate :: Int -> VersionChangeTx Unit
        doMigrate v =
          let migrationsToRun = take (v - oldVersion) (drop oldVersion migrations')
          in void $ sequence (map migration migrationsToRun)

    handle :: forall a. IDBTransaction -> Either Error (Either DOMException a) -> Eff (idb :: IDB | eff) Unit
    handle tx' = case _ of
      -- The Aff failed
      Left _ -> Req.abort tx'
      -- abort the transaction, this will call the error handler on the IndexedDB open request
      Right (Left _) -> Req.abort tx'
      -- All ok, do nothing. The open request onsuccess callback will be called.
      Right _ -> pure unit

-- | Adds a record to a store.
add
  :: forall mode it ir a
   . Store it ir a
  -> Record a
  -> Transaction (write :: Write | mode) Unit
add (Store { name, codec }) item = liftF $ Add (StoreName name) (Codec.encode codec item) unit

clear :: forall mode it ir a. Store it ir a -> Transaction (write :: Write | mode) Unit
clear (Store { name }) = liftF $ Clear (StoreName name) unit

-- | Updates an existing record or adds a new record with a given key.
put
  :: forall mode it ir a
   . Store it ir a
  -> Record a
  -> Transaction (write :: Write | mode) Unit
put (Store { name, codec }) item = liftF
  $ Put (StoreName name) (Codec.encode codec item) unit

-- | Gets a record by the primary key.
get
  :: forall mode it ir a
   . IsKey it
  => Store it ir a
  -> it
  -> Transaction (read :: Read | mode) (Maybe (Record a))
get (Store { name, codec }) key = liftF $ Get (StoreName name) (toKey key) dec
  where
    dec :: Maybe Foreign -> F (Maybe (Record a))
    dec = case _ of
            Nothing -> pure Nothing
            Just item -> pure <$> Codec.decode codec item

-- | Get all records ordered by the primary key
getAll'
  :: forall mode it ir a
   . Store it ir a
  -> Transaction (read :: Read | mode) (Array (Record a))
getAll' (Store { name, codec }) = liftF
  $ GetAll (StoreName name) Nothing (traverse (Codec.decode codec))

-- | Get all records matching a value on the primary key
getAll
  :: forall mode it ir a
  . IsKey it
  => Store it ir a
  -> KeyRange it
  -> Transaction (read :: Read | mode) (Array (Record a))
getAll (Store { name, codec }) v = liftF
  $ GetAll (StoreName name) (Just (Exists.mkExists v)) (traverse (Codec.decode codec))

-- | Deletes a record by the primary key.
delete
  :: forall mode it ir a
   . IsKey it
  => Store it ir a
  -> it
  -> Transaction (write :: Write | mode) Unit
delete (Store { name }) key = liftF $ Delete (StoreName name) (toKey key) unit

-- | Create an object store.
createObjectStore
  :: forall mode it ir rl a
  . R.RowToList ir rl
  => RowListToIndices rl
  => Store it ir a
  -> Transaction (versionchange :: VersionChange | mode) IDBObjectStore
createObjectStore (Store { name, keyPath }) = liftF
  $ CreateObjectStore name (KeyPath keyPath) (rowListToIndices (R.RLProxy :: R.RLProxy rl)) id

createIndex
  :: forall mode
   . StoreName
  -> String
  -> KeyPath
  -> Boolean
  -> Transaction (versionchange :: VersionChange | mode) IDBIndex
createIndex storeName indexName keyPath unique = liftF
  $ CreateIndex storeName indexName keyPath unique id

-- | Class which implements a safe index function for the possible different
-- | index types: unique and non-unique. The return type is different between
-- | unique and non-unique indices: a unique index returns at most one record,
-- | while a non-unique index returns possibly many records.
class IndexQuery (ir :: # Type) (r :: # Type) (label :: Symbol) o where
  index
    :: forall value mode it ignore1 ignore2 ignore3
    . IsKey value
    => IsSymbol label
    => RowCons label value ignore1 r -- get the type of value
    => RowCons label ignore2 ignore3 ir -- check that label is a label in the index row
    => Store it ir r
    -> SProxy label
    -> value
    -> Transaction (read :: Read | mode) (o (Record r))

instance indexQueryUnique
  :: ( RowCons label Unique ir1 ir2 )
  => IndexQuery ir2 r label Maybe where
  index (Store { name, codec }) key v = liftF
    $ IndexUnique (StoreName name) (reflectSymbol key) (toKey v) dec
    where
      dec :: Maybe Foreign -> F (Maybe (Record r))
      dec = case _ of
              Nothing -> pure Nothing
              Just item -> pure <$> Codec.decode codec item

instance indexQueryNonUnique
  :: ( RowCons label NonUnique ir1 ir2 )
  => IndexQuery ir2 r label Array where
  index (Store { name, codec }) key v = liftF
    $ IndexNonUnique
      (StoreName name)
      (reflectSymbol key)
      (toKey v)
      (traverse (Codec.decode codec))

-- The implementation here ensures that all requests are run within
-- the same transaction.
evalTx
  :: forall r eff
   . IDBDatabase
  -> IDBTransaction
  -> TransactionF r
  ~> Request (idb :: IDB | eff)
evalTx idb tx = case _ of
  Add storeName d next -> do
    store <- Req.objectStore storeName tx
    res <- Req.add store d
    pure next
  Clear storeName next -> do
    store <- Req.objectStore storeName tx
    _ <- Req.clear store
    pure next
  Get storeName key f -> do
    store <- Req.objectStore storeName tx
    res <- Req.get store key
    case runExcept (f res) of
      Left e -> liftAff $ throwError (error (show e))
      Right a -> pure a
  GetAll storeName key f -> do
    store <- Req.objectStore storeName tx
    res <- maybe (Req.getAll' store) (Exists.runExists (Req.getAll store)) key
    case runExcept (f res) of
      Left e -> liftAff $ throwError (error (show e))
      Right a -> pure a
  Put storeName d next -> do
    store <- Req.objectStore storeName tx
    Req.put store d
    pure next
  CreateIndex storeName name keyPath unique f -> do
    store <- Req.objectStore storeName tx
    index <- Req.createIndex store name keyPath unique
    pure (f index)
  DeleteIndex storeName indexName next -> do
    store <- Req.objectStore storeName tx
    Req.deleteIndex store indexName
    pure next
  CreateObjectStore storeName keyPath indices f -> do
    store <- Req.createObjectStore idb storeName keyPath
    _ <- traverse (\{ name, unique } ->
      Req.createIndex store name (KeyPath name) unique
    ) indices
    pure (f store)
  Delete storeName key next -> do
    store <- Req.objectStore storeName tx
    Req.delete store key
    pure next
  IndexUnique storeName key v f -> do
    store <- Req.objectStore storeName tx
    res <- Req.index store key v
    case runExcept (f res) of
      Left e -> liftAff $ throwError (error (show e))
      Right a -> pure a
  IndexNonUnique storeName key v f -> do
    store <- Req.objectStore storeName tx
    res <- Req.indexNonUnique store key v
    case runExcept (f res) of
      Left e -> liftAff $ throwError (error (show e))
      Right a -> pure a
