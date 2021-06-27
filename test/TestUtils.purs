module TestUtils where

import Prelude
import Effect (Effect)
import Effect.Console as C
import Data.Argonaut (jsonParser)
import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

foreign import data Queue :: Type -> Type
foreign import newQueue :: ∀ a. Effect (Queue a)
foreign import reset :: ∀ a. Queue a -> Effect Unit
foreign import push :: ∀ a. a -> Queue a -> Effect Unit
foreign import toArray :: ∀ a. Queue a -> Effect (Array a)

foreign import unsafeReadTextFile :: String -> Effect String
foreign import stringHashCode :: String -> Int
foreign import unsafeToJson :: ∀ a. a -> Effect String
foreign import unsafeParseJson :: String -> Effect _

runTest name f = do
  C.log $ "== " <> name <> " =="
  f

unsafeParse x = jsonParser x # unsafePartial fromRight

pureA :: ∀ a. a -> Aff a
pureA x = pure x
notImpl = throw "not implemented" # liftEffect
