module IndexedDb.Type.Row
  ( class HasLabels
  , class RowType
  , class LabelsToList
  , RLProxy(..)
  , labelsToList
  ) where

import Type.Row as R
import Data.List as List
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (kind RowList, Nil, Cons)

data RLProxy (rl ∷ R.RowList) = RLProxy

class LabelsToList (rl ∷ R.RowList) where
  labelsToList ∷ RLProxy rl → List.List String

instance labelsToListNil ∷ LabelsToList R.Nil where
  labelsToList _ = List.Nil

instance labelsToListCons ∷
  ( IsSymbol sym
  , LabelsToList rest)
  ⇒ LabelsToList (R.Cons sym t rest) where
  labelsToList _ = List.Cons
                     (reflectSymbol (SProxy ∷ SProxy sym))
                     (labelsToList (RLProxy ∷ RLProxy rest))


-- | Ensures that the labels on the left side are present in the
-- | row on the right.
class HasLabels (r1 ∷ RowList) (r2 ∷ # Type)

instance hasLabelsNil ∷ HasLabels Nil r

instance hasLabelsCons ∷
  ( RowCons sym t2 r1 r2
  , HasLabels rest r2
  )
  ⇒ HasLabels (Cons sym t rest) r2

-- | A RowList where every element has the same type
class RowType (rl ∷ RowList) (t ∷ Type)

instance rowTypeNil ∷ RowType Nil t

instance rowTypeCons ∷ (RowType rest t) ⇒ RowType (Cons l t rest) t

