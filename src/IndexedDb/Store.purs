module IndexedDb.Store
  ( Store(..)
  , mkStore
  ) where

import Data.Codec.Argonaut as JA
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import IndexedDb.Key (class IsKey)
import Type.Proxy (Proxy)

newtype Store k a = Store { name ∷ String, keyPath ∷ String, codec ∷ JA.JsonCodec a }

mkStore
  ∷ ∀ k a r r1
  . IsSymbol k
  ⇒ IsKey a
  ⇒ RowCons k a r r1
  ⇒ String
  → SProxy k
  → Proxy (Record r)
  → JA.JsonCodec (Record r1)
  → Store a (Record r1)
mkStore name k r codec = Store { name, keyPath: reflectSymbol k, codec }
