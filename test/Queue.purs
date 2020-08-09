module Queue where

import Prelude
import Effect (Effect)

foreign import data Queue :: Type -> Type
foreign import newQueue :: ∀ a. Effect (Queue a)
foreign import reset :: ∀ a. Queue a -> Effect Unit
foreign import push :: ∀ a. a -> Queue a -> Effect Unit
foreign import toArray :: ∀ a. Queue a -> Effect (Array a)

foreign import unsafeReadTextFile :: String -> Effect String
