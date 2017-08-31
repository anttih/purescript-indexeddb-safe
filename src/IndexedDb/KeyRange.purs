module IndexedDb.KeyRange where

foreign import data KeyRange ∷ Type → Type

foreign import lowerBound ∷ ∀ a. a → KeyRange a

foreign import lowerBoundIncluding ∷ ∀ a. a → KeyRange a

foreign import upperBound ∷ ∀ a. a → KeyRange a

foreign import upperBoundIncluding ∷ ∀ a. a → KeyRange a

foreign import bound ∷ ∀ a. a → a → Boolean → Boolean → KeyRange a
