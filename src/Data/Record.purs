module Data.Record where

import Type.Row as R
import Data.List as List
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

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
