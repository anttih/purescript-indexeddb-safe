module DOM.Exception where

import Control.Monad.Eff.Exception as Error
import Unsafe.Coerce (unsafeCoerce)

foreign import data DOMException ∷ Type

message ∷ DOMException → String
message = unsafeCoerce Error.message

