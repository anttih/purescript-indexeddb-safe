module IndexedDb.Store
  ( Store(..)
  , class HasLabels
  , class RowType
  , Unique
  , mkStore
  ) where

import Data.Codec.Argonaut as JA
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import IndexedDb.Key (class IsKey)
import Type.Row (class RowToList, kind RowList, Nil, Cons)

class HasLabels (r1 ∷ RowList) (r2 ∷ # Type)

instance hasLabelsNil ∷ HasLabels Nil r

instance hasLabelsCons ∷
  ( RowCons sym t2 r1 r2
  , HasLabels rest r2
  )
  ⇒ HasLabels (Cons sym t rest) r2

class RowType (rl ∷ RowList) (t ∷ Type)

instance rowTypeNil ∷ RowType Nil t

instance rowTypeCons ∷ (RowType rest t) ⇒ RowType (Cons l t rest) t

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
