module IndexedDb.Index where

import Data.List as List
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import IndexedDb.Type.Row (RLProxy(..))
import Type.Row as R

data Unique
data NonUnique

class LabelsToList (rl ∷ R.RowList) where
  labelsToList ∷ RLProxy rl → List.List { name ∷ String, unique ∷ Boolean }

instance labelsToListNil ∷ LabelsToList R.Nil where
  labelsToList _ = List.Nil

instance labelsToListConsUnique ∷
  ( IsSymbol sym
  , LabelsToList rest)
  ⇒ LabelsToList (R.Cons sym Unique rest) where
    labelsToList _ = List.Cons { name: (reflectSymbol (SProxy ∷ SProxy sym))
                               , unique: true
                               }
                     (labelsToList (RLProxy ∷ RLProxy rest))


instance labelsToListConsNonUnique ∷
  ( IsSymbol sym
  , LabelsToList rest)
  ⇒ LabelsToList (R.Cons sym NonUnique rest) where
    labelsToList _ = List.Cons { name: (reflectSymbol (SProxy ∷ SProxy sym))
                               , unique: false
                               }
                     (labelsToList (RLProxy ∷ RLProxy rest))


