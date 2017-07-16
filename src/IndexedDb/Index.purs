module IndexedDb.Index where

import Data.List as List
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import IndexedDb.Type.Row (RLProxy(..))
import Type.Row as R

-- | Type for unique indices
data Unique

-- | Type for non-unique indices
data NonUnique

-- | Transforms a type-level row of index definitions into
-- | a term-level list of indices.
class RowListToIndices (rl ∷ R.RowList) where
  rowListToIndices ∷ RLProxy rl → List.List { name ∷ String, unique ∷ Boolean }

instance rowListToIndexTypesNil ∷ RowListToIndices R.Nil where
  rowListToIndices _ = List.Nil

instance rowListToIndexTypesConsUnique ∷
  ( IsSymbol sym
  , RowListToIndices rest)
  ⇒ RowListToIndices (R.Cons sym Unique rest) where
    rowListToIndices _ = List.Cons { name: (reflectSymbol (SProxy ∷ SProxy sym))
                               , unique: true
                               }
                     (rowListToIndices (RLProxy ∷ RLProxy rest))


instance rowListToIndexTypesConsNonUnique ∷
  ( IsSymbol sym
  , RowListToIndices rest)
  ⇒ RowListToIndices (R.Cons sym NonUnique rest) where
    rowListToIndices _ = List.Cons { name: (reflectSymbol (SProxy ∷ SProxy sym))
                               , unique: false
                               }
                     (rowListToIndices (RLProxy ∷ RLProxy rest))


