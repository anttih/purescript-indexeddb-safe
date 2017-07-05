module IndexedDb.Request
  ( Request
  , abort
  , add
  , close
  , createObjectStore
  , deleteDatabase
  , delete
  , get
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
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import IndexedDb.Key (Key)
import IndexedDb.Types (Database, TxMode, IDB, IDBDatabase, IDBObjectStore, IDBTransaction, KeyPath, StoreName, Version, VersionChangeEventInit)
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
open db v f = makeRequest (openImpl db v f)

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
transaction db stores flag = makeRequest (transactionImpl db stores flag)

objectStore ∷ ∀ eff. StoreName → IDBTransaction → Request (idb :: IDB | eff) IDBObjectStore
objectStore store tsx = liftEff (objectStoreImpl store tsx)

add ∷ ∀ eff. IDBObjectStore → Json → Request (idb ∷ IDB | eff) Unit
add store item = makeRequest (addImpl store item)

get ∷ ∀ eff. IDBObjectStore → Key → Request (idb ∷ IDB | eff) (Maybe Json)
get store key = makeRequest (getImpl Nothing Just store key)

put ∷ ∀ eff. IDBObjectStore → Json → Request (idb ∷ IDB | eff) Unit
put store item = makeRequest (putImpl store item)

createObjectStore
  ∷ ∀ eff
  . IDBDatabase
  → String
  → KeyPath
  → Request (idb ∷ IDB | eff) IDBObjectStore
createObjectStore idb store key = ExceptT
  $ liftEff
  $ createObjectStoreImpl Left Right idb store key

delete ∷ ∀ eff. IDBObjectStore → Key → Request (idb ∷ IDB | eff) Unit
delete store key = makeRequest (deleteImpl store key)

foreign import openImpl
  ∷ forall eff
  . Database
  → Version
  → (VersionChangeEventInit → IDBDatabase → IDBTransaction → Eff (idb ∷ IDB | eff) Unit)
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (IDBDatabase → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import closeImpl ∷ ∀ eff. IDBDatabase → Eff (idb ∷ IDB | eff) Unit

foreign import deleteDatabaseImpl
  ∷ forall eff
  . Database
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import transactionImpl
  ∷ forall eff
  . IDBDatabase
  → Array StoreName
  → TxMode
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (IDBTransaction → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import objectStoreImpl
  ∷ forall eff
  . StoreName
  → IDBTransaction
  → Eff (idb ∷ IDB | eff) IDBObjectStore

foreign import addImpl
  ∷ forall eff
  . IDBObjectStore
  → Json
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import getImpl
  ∷ forall eff a
  . Maybe a -- The value Nothing
  → (a → Maybe a) -- Just
  → IDBObjectStore
  → Key
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Maybe Json → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import putImpl
  ∷ forall eff
  . IDBObjectStore
  → Json
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import createObjectStoreImpl
  ∷ forall eff
  . (DOMException → Either DOMException IDBObjectStore)
  → (IDBObjectStore → Either DOMException IDBObjectStore)
  → IDBDatabase
  → String
  → KeyPath
  → Eff (idb ∷ IDB | eff) (Either DOMException IDBObjectStore)

foreign import deleteImpl
  ∷ forall eff
  . IDBObjectStore
  → Key
  → (DOMException → Eff (idb ∷ IDB | eff) Unit)
  → (Unit → Eff (idb ∷ IDB | eff) Unit)
  → Eff (idb ∷ IDB | eff) Unit

foreign import abort ∷ ∀ eff. IDBTransaction → Eff (idb ∷ IDB | eff) Unit
