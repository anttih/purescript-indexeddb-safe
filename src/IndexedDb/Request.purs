module IndexedDb.Request
  ( Request
  , abort
  , add
  , close
  , createObjectStore
  , createIndex
  , deleteDatabase
  , delete
  , get
  , getAll'
  , getAll
  , index
  , indexNonUnique
  , put
  , objectStore
  , open
  , transaction
  ) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (ExceptT(..))
import DOM.Exception (DOMException)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, Fn4, Fn5, Fn6, Fn7, runFn3, runFn4, runFn5, runFn6, runFn7)
import Data.Maybe (Maybe(..))
import IndexedDb.Key (Key)
import IndexedDb.KeyRange (KeyRange)
import IndexedDb.Types (Database, IDB, IDBDatabase, IDBIndex, IDBObjectStore, IDBTransaction, KeyPath, StoreName, TxMode, Version, VersionChangeEventInit)
import Prelude hiding (add)

type Request eff a = ExceptT DOMException (Aff eff) a

makeRequest
  ∷ ∀ eff a
  . ((DOMException → Eff (idb ∷ IDB | eff) Unit)
    → (a → Eff (idb ∷ IDB | eff) Unit)
    → Eff (idb ∷ IDB | eff) Unit
    )
  → Request (idb ∷ IDB | eff) a
makeRequest f = ExceptT $ makeAff \error success → do
  f (success <<< Left) (success <<< Right)

open
  ∷ ∀ eff
  . Database
  → Version
  → (VersionChangeEventInit → IDBDatabase → IDBTransaction → Eff (idb ∷ IDB | eff) Unit)
  → Request (idb ∷ IDB | eff) IDBDatabase
open db v f = makeRequest (\e s → runFn5 openImpl db v f e s)

close ∷ ∀ eff. IDBDatabase → Request (idb ∷ IDB | eff) Unit
close = liftEff <<< closeImpl

deleteDatabase ∷ ∀ eff. Database → Request (idb ∷ IDB | eff) Unit
deleteDatabase db = makeRequest (deleteDatabaseImpl db)

transaction
  ∷ ∀ eff
  . IDBDatabase
  → Array StoreName
  → TxMode
  → Request (idb ∷ IDB | eff) IDBTransaction
transaction db stores flag = makeRequest
  (\e s → runFn5 transactionImpl db stores flag e s)

objectStore ∷ ∀ eff. StoreName → IDBTransaction → Request (idb :: IDB | eff) IDBObjectStore
objectStore store tsx = liftEff (objectStoreImpl store tsx)

add ∷ ∀ eff. IDBObjectStore → Foreign → Request (idb ∷ IDB | eff) Unit
add store item = makeRequest (addImpl store item)

get ∷ ∀ eff. IDBObjectStore → Key → Request (idb ∷ IDB | eff) (Maybe Foreign)
get store key = makeRequest (\e s → runFn6 getImpl Nothing Just store key e s)

getAll' ∷ ∀ eff. IDBObjectStore → Request (idb ∷ IDB | eff) (Array Foreign)
getAll' store = makeRequest (\e s → runFn3 getAllImpl store e s)

getAll ∷ ∀ eff a. IDBObjectStore → KeyRange a → Request (idb ∷ IDB | eff) (Array Foreign)
getAll store key = makeRequest (\e s → runFn4 getAllByKeyImpl store key e s)

index ∷ ∀ eff. IDBObjectStore → String → Key → Request (idb ∷ IDB | eff) (Maybe Foreign)
index store indexName v = makeRequest
  (\e s → runFn7 indexImpl Nothing Just store indexName v e s)

indexNonUnique ∷ ∀ eff. IDBObjectStore → String → Key → Request (idb ∷ IDB | eff) (Array Foreign)
indexNonUnique store indexName v = makeRequest
  (\e s → runFn5 indexNonUniqueImpl store indexName v e s)

put ∷ ∀ eff. IDBObjectStore → Foreign → Request (idb ∷ IDB | eff) Unit
put store item = makeRequest (putImpl store item)

createObjectStore
  ∷ ∀ eff
  . IDBDatabase
  → String
  → KeyPath
  → Request (idb ∷ IDB | eff) IDBObjectStore
createObjectStore idb store key = ExceptT
  $ liftEff
  $ runFn5 createObjectStoreImpl Left Right idb store key

createIndex
  ∷ ∀ eff
  . IDBObjectStore
  → String
  → KeyPath
  → Boolean
  → Request (idb ∷ IDB | eff) IDBIndex
createIndex store indexName path unique = ExceptT
  $ liftEff
  $ runFn6 createIndexImpl Left Right store indexName path unique

delete ∷ ∀ eff. IDBObjectStore → Key → Request (idb ∷ IDB | eff) Unit
delete store key = makeRequest (deleteImpl store key)

foreign import openImpl
  ∷ forall eff.
  Fn5
  Database
  Version
  (VersionChangeEventInit → IDBDatabase → IDBTransaction → Eff (idb ∷ IDB | eff) Unit)
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (IDBDatabase → Eff (idb ∷ IDB | eff) Unit)
  (Eff (idb ∷ IDB | eff) Unit)

foreign import closeImpl ∷ ∀ eff. IDBDatabase → Eff (idb ∷ IDB | eff) Unit

foreign import deleteDatabaseImpl
  ∷ forall eff
  . Database
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import transactionImpl
  ∷ forall eff.
  Fn5
  IDBDatabase
  (Array StoreName)
  TxMode
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (IDBTransaction → Eff (idb ∷ IDB | eff) Unit)
  (Eff (idb ∷ IDB | eff) Unit)

foreign import objectStoreImpl
  ∷ forall eff
  . StoreName
  → IDBTransaction
  → Eff (idb ∷ IDB | eff) IDBObjectStore

foreign import addImpl
  ∷ forall eff
  . IDBObjectStore
  → Foreign
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import getImpl
  ∷ forall eff a.
  Fn6
  (Maybe a) -- The value Nothing
  (a → Maybe a) -- Just
  IDBObjectStore
  Key
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Maybe Foreign → Eff (idb ∷ IDB | eff) Unit)
  (Eff (idb ∷ IDB | eff) Unit)

foreign import getAllImpl
  ∷ forall eff.
  Fn3
  IDBObjectStore
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Array Foreign → Eff (idb ∷ IDB | eff) Unit)
  (Eff (idb ∷ IDB | eff) Unit)

foreign import getAllByKeyImpl
  ∷ forall eff a.
  Fn4
  IDBObjectStore
  (KeyRange a)
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Array Foreign → Eff (idb ∷ IDB | eff) Unit)
  (Eff (idb ∷ IDB | eff) Unit)

foreign import indexImpl
  ∷ forall eff a.
  Fn7
  (Maybe a) -- The value Nothing
  (a → Maybe a) -- Just
  IDBObjectStore
  String -- the index name
  Key -- the value, really should be Foreign
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Maybe Foreign → Eff (idb ∷ IDB | eff) Unit)
  (Eff (idb ∷ IDB | eff) Unit)

foreign import indexNonUniqueImpl
  ∷ forall eff.
  Fn5
  IDBObjectStore
  String -- the index name
  Key -- the value, really should be Foreign
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Array Foreign → Eff (idb ∷ IDB | eff) Unit)
  (Eff (idb ∷ IDB | eff) Unit)

foreign import putImpl
  ∷ forall eff
  . IDBObjectStore
  → Foreign
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import createObjectStoreImpl
  ∷ forall eff.
  Fn5
  (DOMException → Either DOMException IDBObjectStore)
  (IDBObjectStore → Either DOMException IDBObjectStore)
  IDBDatabase
  String
  KeyPath
  (Eff (idb ∷ IDB | eff) (Either DOMException IDBObjectStore))

foreign import createIndexImpl
  ∷ forall eff.
  Fn6
  (DOMException → Either DOMException IDBObjectStore)
  (IDBObjectStore → Either DOMException IDBObjectStore)
  IDBObjectStore
  String
  KeyPath
  Boolean
  (Eff (idb ∷ IDB | eff) (Either DOMException IDBIndex))

foreign import deleteImpl
  ∷ forall eff
  . IDBObjectStore
  → Key
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import abort ∷ ∀ eff. IDBTransaction → Eff (idb ∷ IDB | eff) Unit
