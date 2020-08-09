module Test.Main (main) where

import Prelude
import Test.Assert

import Data.Argonaut.Parser (jsonParser)
import Data.Either (fromRight)
import Effect (Effect)
import Effect.Now (nowDateTime)
import Partial.Unsafe (unsafePartial)
import Test.Domain2 as TD2
import Test.PeriodicPostsImages as T3

assertUrl url =
  assertEqual 
    { expected: "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=_KEY_&tag=cat"
    , actual: url }

unsafeParse x = jsonParser x # unsafePartial fromRight

main :: Effect Unit
main = do
  now <- nowDateTime
  TD2.main
  T3.test
