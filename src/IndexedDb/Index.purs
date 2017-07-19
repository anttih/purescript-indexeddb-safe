module IndexedDb.Index where

import Data.List as List
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (kind RowList, RLProxy(..), Nil, Cons)

-- | Type for a unique index
data Unique

-- | Type for a non-unique index
data NonUnique

-- | Transforms a type-level row of index definitions into
-- | a term-level list of indices.
class RowListToIndices (rl ∷ RowList) where
  rowListToIndices ∷ RLProxy rl → List.List { name ∷ String, unique ∷ Boolean }

instance rowListToIndexTypesNil ∷ RowListToIndices Nil where
  rowListToIndices _ = List.Nil

instance rowListToIndexTypesConsUnique ∷
  ( IsSymbol sym
  , RowListToIndices rest)
  ⇒ RowListToIndices (Cons sym Unique rest) where
    rowListToIndices _ = List.Cons { name: reflectSymbol (SProxy ∷ SProxy sym)
                                   , unique: true
                                   }
                     (rowListToIndices (RLProxy ∷ RLProxy rest))


instance rowListToIndexTypesConsNonUnique ∷
  ( IsSymbol sym
  , RowListToIndices rest)
  ⇒ RowListToIndices (Cons sym NonUnique rest) where
    rowListToIndices _ = List.Cons { name: reflectSymbol (SProxy ∷ SProxy sym)
                                   , unique: false
                                   }
                     (rowListToIndices (RLProxy ∷ RLProxy rest))


