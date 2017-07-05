module IndexedDb.Key where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Key ∷ Type

class IsKey k where
  toKey ∷ k → Key

instance isKeyInt ∷ IsKey Int where
  toKey = unsafeCoerce

instance isKeyNumber ∷ IsKey Number where
  toKey = unsafeCoerce

instance isKeyString ∷ IsKey String where
  toKey = unsafeCoerce
