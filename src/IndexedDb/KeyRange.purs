module IndexedDb.KeyRange where

foreign import data KeyRange :: Type -> Type

foreign import lowerBound :: forall a. a -> KeyRange a

foreign import lowerBoundIncluding :: forall a. a -> KeyRange a

foreign import upperBound :: forall a. a -> KeyRange a

foreign import upperBoundIncluding :: forall a. a -> KeyRange a

foreign import bound :: forall a. a -> a -> Boolean -> Boolean -> KeyRange a
