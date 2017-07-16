module IndexedDb.Store
  ( Store(..)
  , mkStore
  ) where

import Data.Codec.Argonaut as JA
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import IndexedDb.Key (class IsKey)
import IndexedDb.Type.Row (class HasLabels)
import Type.Row (class RowToList)

-- foreign import kind IndexType
-- foreign import data Unique ∷ IndexType

-- newtype Codec a = Codec { encode ∷ a → Foreign
--                         , decode ∷ Foreign → F a
--                         }

newtype Store k (ir ∷ # Type) (r ∷ # Type)
  = Store { name ∷ String, keyPath ∷ String, codec ∷ JA.JsonCodec (Record r) }

mkStore
  ∷ ∀ k a ir rl r r1
  . IsSymbol k
  ⇒ IsKey a
  ⇒ RowCons k a r1 r
  ⇒ RowToList ir rl
  ⇒ HasLabels rl r
  ⇒ String
  → SProxy k
  → JA.JsonCodec (Record r)
  → Store a ir r
mkStore name k codec = Store { name, keyPath: reflectSymbol k, codec }
