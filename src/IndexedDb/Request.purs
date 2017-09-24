module IndexedDb.Request
  ( Request
  , abort
  , add
  , clear
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
import Control.Monad.Eff.Uncurried (runEffFn2, runEffFn3, runEffFn4, runEffFn5, runEffFn6, runEffFn7)
import Control.Monad.Except (ExceptT(..))
import DOM.Exception (DOMException)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import IndexedDb.Key (Key)
import IndexedDb.KeyRange (KeyRange)
import IndexedDb.Request.Internal as Internal
import IndexedDb.Types (Database, IDB, IDBDatabase, IDBIndex, IDBObjectStore, IDBTransaction, KeyPath, StoreName, TxMode, Version, VersionChangeEventInit)
import Prelude hiding (add)

abort = Internal.abort

type Request eff a = ExceptT DOMException (Aff eff) a

makeRequest
  :: forall eff a
  . ((DOMException -> Eff (idb :: IDB | eff) Unit)
    -> (a -> Eff (idb :: IDB | eff) Unit)
    -> Eff (idb :: IDB | eff) Unit
    )
  -> Request (idb :: IDB | eff) a
makeRequest f = ExceptT $ makeAff \error success -> do
  f (success <<< Left) (success <<< Right)

open
  :: forall eff
  . Database
  -> Version
  -> (VersionChangeEventInit -> IDBDatabase -> IDBTransaction -> Eff (idb :: IDB | eff) Unit)
  -> Request (idb :: IDB | eff) IDBDatabase
open db v f = makeRequest (\e s -> runEffFn5 Internal.open db v f e s)

close :: forall eff. IDBDatabase -> Request (idb :: IDB | eff) Unit
close = liftEff <<< Internal.close

deleteDatabase :: forall eff. Database -> Request (idb :: IDB | eff) Unit
deleteDatabase db = makeRequest (runEffFn3 Internal.deleteDatabase db)

transaction
  :: forall eff
  . IDBDatabase
  -> Array StoreName
  -> TxMode
  -> Request (idb :: IDB | eff) IDBTransaction
transaction db stores flag = makeRequest
  (\e s -> runEffFn5 Internal.transaction db stores flag e s)

objectStore :: forall eff. StoreName -> IDBTransaction -> Request (idb :: IDB | eff) IDBObjectStore
objectStore store tsx = liftEff (runEffFn2 Internal.objectStore store tsx)

add :: forall eff. IDBObjectStore -> Foreign -> Request (idb :: IDB | eff) Unit
add store item = makeRequest (runEffFn4 Internal.add store item)

clear :: forall eff. IDBObjectStore -> Request (idb :: IDB | eff) Unit
clear store = makeRequest (\e s -> runEffFn3 Internal.clear store e s)

get :: forall eff. IDBObjectStore -> Key -> Request (idb :: IDB | eff) (Maybe Foreign)
get store key = makeRequest (\e s -> runEffFn6 Internal.get Nothing Just store key e s)

getAll' :: forall eff. IDBObjectStore -> Request (idb :: IDB | eff) (Array Foreign)
getAll' store = makeRequest (\e s -> runEffFn3 Internal.getAll store e s)

getAll :: forall eff a. IDBObjectStore -> KeyRange a -> Request (idb :: IDB | eff) (Array Foreign)
getAll store key = makeRequest (\e s -> runEffFn4 Internal.getAllByKey store key e s)

index :: forall eff. IDBObjectStore -> String -> Key -> Request (idb :: IDB | eff) (Maybe Foreign)
index store indexName v = makeRequest
  (\e s -> runEffFn7 Internal.index Nothing Just store indexName v e s)

indexNonUnique :: forall eff. IDBObjectStore -> String -> Key -> Request (idb :: IDB | eff) (Array Foreign)
indexNonUnique store indexName v = makeRequest
  (\e s -> runEffFn5 Internal.indexNonUnique store indexName v e s)

put :: forall eff. IDBObjectStore -> Foreign -> Request (idb :: IDB | eff) Unit
put store item = makeRequest (runEffFn4 Internal.put store item)

createObjectStore
  :: forall eff
  . IDBDatabase
  -> String
  -> KeyPath
  -> Request (idb :: IDB | eff) IDBObjectStore
createObjectStore idb store key = ExceptT
  $ liftEff
  $ runEffFn5 Internal.createObjectStore Left Right idb store key

createIndex
  :: forall eff
  . IDBObjectStore
  -> String
  -> KeyPath
  -> Boolean
  -> Request (idb :: IDB | eff) IDBIndex
createIndex store indexName path unique = ExceptT
  $ liftEff
  $ runEffFn6 Internal.createIndex Left Right store indexName path unique

delete :: forall eff. IDBObjectStore -> Key -> Request (idb :: IDB | eff) Unit
delete store key = makeRequest (runEffFn4 Internal.delete store key)
