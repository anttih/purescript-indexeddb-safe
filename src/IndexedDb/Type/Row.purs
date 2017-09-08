module IndexedDb.Type.Row
  ( class HasLabels
  , class RowType
  ) where

import Type.Row (kind RowList, Nil, Cons)

-- | Ensures that the labels on the left side are present in the
-- | row on the right.
class HasLabels (r1 :: RowList) (r2 :: # Type)

instance hasLabelsNil :: HasLabels Nil r

instance hasLabelsCons ::
  ( RowCons sym t2 r1 r2
  , HasLabels rest r2
  )
  => HasLabels (Cons sym t rest) r2

-- | A RowList where every element has the same type
class RowType (rl :: RowList) (t :: Type)

instance rowTypeNil :: RowType Nil t

instance rowTypeCons :: (RowType rest t) => RowType (Cons l t rest) t

