module IndexedDb.Types where

import Control.Monad.Eff (kind Effect)
import Data.Maybe (Maybe)

foreign import data IDB ∷ Effect
foreign import data IDBDatabase ∷ Type
foreign import data IDBObjectStore ∷ Type
foreign import data IDBTransaction ∷ Type

newtype Database = Database String
newtype Version = Version Int
newtype StoreName = StoreName String
newtype KeyPath = KeyPath String
newtype TxMode = TxMode String

newtype VersionChangeEventInit
  = VersionChangeEventInit { oldVersion ∷ Int
                           , newVersion ∷ Maybe Int
                           }

