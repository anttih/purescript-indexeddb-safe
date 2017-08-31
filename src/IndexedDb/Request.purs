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
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, EffFn4, EffFn5, EffFn6, EffFn7, runEffFn2, runEffFn3, runEffFn4, runEffFn5, runEffFn6, runEffFn7)
import Control.Monad.Except (ExceptT(..))
import DOM.Exception (DOMException)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
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
open db v f = makeRequest (\e s → runEffFn5 openImpl db v f e s)

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
  (\e s → runEffFn5 transactionImpl db stores flag e s)

objectStore ∷ ∀ eff. StoreName → IDBTransaction → Request (idb :: IDB | eff) IDBObjectStore
objectStore store tsx = liftEff (runEffFn2 objectStoreImpl store tsx)

add ∷ ∀ eff. IDBObjectStore → Foreign → Request (idb ∷ IDB | eff) Unit
add store item = makeRequest (runEffFn4 addImpl store item)

get ∷ ∀ eff. IDBObjectStore → Key → Request (idb ∷ IDB | eff) (Maybe Foreign)
get store key = makeRequest (\e s → runEffFn6 getImpl Nothing Just store key e s)

getAll' ∷ ∀ eff. IDBObjectStore → Request (idb ∷ IDB | eff) (Array Foreign)
getAll' store = makeRequest (\e s → runEffFn3 getAllImpl store e s)

getAll ∷ ∀ eff a. IDBObjectStore → KeyRange a → Request (idb ∷ IDB | eff) (Array Foreign)
getAll store key = makeRequest (\e s → runEffFn4 getAllByKeyImpl store key e s)

index ∷ ∀ eff. IDBObjectStore → String → Key → Request (idb ∷ IDB | eff) (Maybe Foreign)
index store indexName v = makeRequest
  (\e s → runEffFn7 indexImpl Nothing Just store indexName v e s)

indexNonUnique ∷ ∀ eff. IDBObjectStore → String → Key → Request (idb ∷ IDB | eff) (Array Foreign)
indexNonUnique store indexName v = makeRequest
  (\e s → runEffFn5 indexNonUniqueImpl store indexName v e s)

put ∷ ∀ eff. IDBObjectStore → Foreign → Request (idb ∷ IDB | eff) Unit
put store item = makeRequest (runEffFn4 putImpl store item)

createObjectStore
  ∷ ∀ eff
  . IDBDatabase
  → String
  → KeyPath
  → Request (idb ∷ IDB | eff) IDBObjectStore
createObjectStore idb store key = ExceptT
  $ liftEff
  $ runEffFn5 createObjectStoreImpl Left Right idb store key

createIndex
  ∷ ∀ eff
  . IDBObjectStore
  → String
  → KeyPath
  → Boolean
  → Request (idb ∷ IDB | eff) IDBIndex
createIndex store indexName path unique = ExceptT
  $ liftEff
  $ runEffFn6 createIndexImpl Left Right store indexName path unique

delete ∷ ∀ eff. IDBObjectStore → Key → Request (idb ∷ IDB | eff) Unit
delete store key = makeRequest (runEffFn4 deleteImpl store key)

foreign import openImpl
  ∷ forall eff.
  EffFn5 (idb ∷ IDB | eff)
  Database
  Version
  (VersionChangeEventInit → IDBDatabase → IDBTransaction → Eff (idb ∷ IDB | eff) Unit)
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (IDBDatabase → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import closeImpl ∷ ∀ eff. IDBDatabase → Eff (idb ∷ IDB | eff) Unit

foreign import deleteDatabaseImpl
  ∷ forall eff
  . Database
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import transactionImpl
  ∷ forall eff.
  EffFn5
  (idb ∷ IDB | eff)
  IDBDatabase
  (Array StoreName)
  TxMode
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (IDBTransaction → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import objectStoreImpl
  ∷ forall eff.
  EffFn2
  (idb ∷ IDB | eff)
  StoreName
  IDBTransaction
  IDBObjectStore

foreign import addImpl
  ∷ forall eff.
  EffFn4
  (idb ∷ IDB | eff)
  IDBObjectStore
  Foreign
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Unit → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import getImpl
  ∷ forall eff a.
  EffFn6 (idb ∷ IDB | eff)
  (Maybe a) -- The value Nothing
  (a → Maybe a) -- Just
  IDBObjectStore
  Key
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Maybe Foreign → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import getAllImpl
  ∷ forall eff.
  EffFn3 (idb ∷ IDB | eff)
  IDBObjectStore
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Array Foreign → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import getAllByKeyImpl
  ∷ forall eff a.
  EffFn4 (idb ∷ IDB | eff)
  IDBObjectStore
  (KeyRange a)
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Array Foreign → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import indexImpl
  ∷ forall eff a.
  EffFn7 (idb ∷ IDB | eff)
  (Maybe a) -- The value Nothing
  (a → Maybe a) -- Just
  IDBObjectStore
  String -- the index name
  Key -- the value, really should be Foreign
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Maybe Foreign → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import indexNonUniqueImpl
  ∷ forall eff.
  EffFn5 (idb ∷ IDB | eff)
  IDBObjectStore
  String -- the index name
  Key -- the value, really should be Foreign
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Array Foreign → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import putImpl
  ∷ forall eff.
  EffFn4 (idb ∷ IDB | eff)
  IDBObjectStore
  Foreign
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Unit → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import createObjectStoreImpl
  ∷ forall eff.
  EffFn5 (idb ∷ IDB | eff)
  (DOMException → Either DOMException IDBObjectStore)
  (IDBObjectStore → Either DOMException IDBObjectStore)
  IDBDatabase
  String
  KeyPath
  (Either DOMException IDBObjectStore)

foreign import createIndexImpl
  ∷ forall eff.
  EffFn6 (idb ∷ IDB | eff)
  (DOMException → Either DOMException IDBObjectStore)
  (IDBObjectStore → Either DOMException IDBObjectStore)
  IDBObjectStore
  String
  KeyPath
  Boolean
  (Either DOMException IDBIndex)

foreign import deleteImpl
  ∷ forall eff.
  EffFn4 (idb ∷ IDB | eff)
  IDBObjectStore
  Key
  (DOMException → Eff (idb ∷ IDB | eff) Unit)
  (Unit → Eff (idb ∷ IDB | eff) Unit)
  Unit

foreign import abort ∷ ∀ eff. IDBTransaction → Eff (idb ∷ IDB | eff) Unit
