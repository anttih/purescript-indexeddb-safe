module IndexedDb.Store
  ( Store(..)
  , Unique
  , mkStore
  ) where

import Data.Codec.Argonaut as JA
import Data.Record (class HasLabels, class RowType)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import IndexedDb.Key (class IsKey)
import Type.Row (class RowToList)

data Unique = Unique
-- foreign import kind IndexType
-- foreign import data Unique ∷ IndexType

newtype Store k ir a = Store { name ∷ String, keyPath ∷ String, codec ∷ JA.JsonCodec a }

mkStore
  ∷ ∀ k a ir rl r r1
  . IsSymbol k
  ⇒ IsKey a
  ⇒ RowCons k a r1 r
  ⇒ RowToList ir rl
  ⇒ HasLabels rl r
  ⇒ RowType rl Unique
  ⇒ String
  → SProxy k
  → JA.JsonCodec (Record r)
  → Store a (Record ir) (Record r)
mkStore name k codec = Store { name, keyPath: reflectSymbol k, codec }
