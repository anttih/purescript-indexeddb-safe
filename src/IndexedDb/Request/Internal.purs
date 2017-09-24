module IndexedDb.Request.Internal where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, EffFn4, EffFn5, EffFn6, EffFn7)
import DOM.Exception (DOMException)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import IndexedDb.Key (Key)
import IndexedDb.KeyRange (KeyRange)
import IndexedDb.Types (Database, IDB, IDBDatabase, IDBIndex, IDBObjectStore, IDBTransaction, KeyPath, StoreName, TxMode, Version, VersionChangeEventInit)
import Prelude hiding (add)

foreign import open
  :: forall eff.
  EffFn5 (idb :: IDB | eff)
  Database
  Version
  (VersionChangeEventInit -> IDBDatabase -> IDBTransaction -> Eff (idb :: IDB | eff) Unit)
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (IDBDatabase -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import close :: forall eff. IDBDatabase -> Eff (idb :: IDB | eff) Unit

foreign import deleteDatabase
  :: forall eff.
  EffFn3 (idb :: IDB | eff)
  Database
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Unit -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import transaction
  :: forall eff.
  EffFn5
  (idb :: IDB | eff)
  IDBDatabase
  (Array StoreName)
  TxMode
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (IDBTransaction -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import objectStore
  :: forall eff.
  EffFn2
  (idb :: IDB | eff)
  StoreName
  IDBTransaction
  IDBObjectStore

foreign import add
  :: forall eff.
  EffFn4
  (idb :: IDB | eff)
  IDBObjectStore
  Foreign
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Unit -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import clear
  :: forall eff.
  EffFn3
  (idb :: IDB | eff)
  IDBObjectStore
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Unit -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import get
  :: forall eff a.
  EffFn6 (idb :: IDB | eff)
  (Maybe a) -- The value Nothing
  (a -> Maybe a) -- Just
  IDBObjectStore
  Key
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Maybe Foreign -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import getAll
  :: forall eff.
  EffFn3 (idb :: IDB | eff)
  IDBObjectStore
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Array Foreign -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import getAllByKey
  :: forall eff a.
  EffFn4 (idb :: IDB | eff)
  IDBObjectStore
  (KeyRange a)
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Array Foreign -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import index
  :: forall eff a.
  EffFn7 (idb :: IDB | eff)
  (Maybe a) -- The value Nothing
  (a -> Maybe a) -- Just
  IDBObjectStore
  String -- the index name
  Key -- the value, really should be Foreign
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Maybe Foreign -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import indexNonUnique
  :: forall eff.
  EffFn5 (idb :: IDB | eff)
  IDBObjectStore
  String -- the index name
  Key -- the value, really should be Foreign
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Array Foreign -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import put
  :: forall eff.
  EffFn4 (idb :: IDB | eff)
  IDBObjectStore
  Foreign
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Unit -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import createObjectStore
  :: forall eff.
  EffFn5 (idb :: IDB | eff)
  (DOMException -> Either DOMException IDBObjectStore)
  (IDBObjectStore -> Either DOMException IDBObjectStore)
  IDBDatabase
  String
  KeyPath
  (Either DOMException IDBObjectStore)

foreign import createIndex
  :: forall eff.
  EffFn6 (idb :: IDB | eff)
  (DOMException -> Either DOMException IDBObjectStore)
  (IDBObjectStore -> Either DOMException IDBObjectStore)
  IDBObjectStore
  String
  KeyPath
  Boolean
  (Either DOMException IDBIndex)

foreign import delete
  :: forall eff.
  EffFn4 (idb :: IDB | eff)
  IDBObjectStore
  Key
  (DOMException -> Eff (idb :: IDB | eff) Unit)
  (Unit -> Eff (idb :: IDB | eff) Unit)
  Unit

foreign import abort :: forall eff. IDBTransaction -> Eff (idb :: IDB | eff) Unit
