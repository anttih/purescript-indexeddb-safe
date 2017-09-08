module IndexedDb.Store
  ( Store(..)
  , ForeignCodec
  , mkStore
  ) where

import Data.Codec (BasicCodec)
import Data.Foreign (F, Foreign)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import IndexedDb.Key (class IsKey)
import IndexedDb.Type.Row (class HasLabels)
import Type.Row (class RowToList)

type ForeignCodec a = BasicCodec F Foreign a

newtype Store k (ir :: # Type) (r :: # Type)
  = Store { name :: String, keyPath :: String, codec :: ForeignCodec (Record r) }

mkStore
  :: forall k a ir rl r r1
  . IsSymbol k
  => IsKey a
  => RowCons k a r1 r
  => RowToList ir rl
  => HasLabels rl r
  => String
  -> SProxy k
  -> ForeignCodec (Record r)
  -> Store a ir r
mkStore name k codec = Store { name, keyPath: reflectSymbol k, codec }
