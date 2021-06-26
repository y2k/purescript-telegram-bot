module Test.Common (main) where

import Prelude

import Common as C
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull)
import Domain as D
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Main (unsafeParseJson, unsafeToJson)
import Test.Assert (assert', assertEqual)
import TestUtils (runTest)
import TestUtils as T

main :: Effect Unit
main = do
  runTest "Common tryExtractCommand /dog"
    $ assertEqual
        { actual : C.tryExtractCommand { text : notNull "/dog" },
          expected : Just [ "/dog" ] }
  runTest "Common tryExtractCommand /dog@mybot"
    $ assertEqual
        { actual : C.tryExtractCommand { text : notNull "/dog@mybot" },
          expected : Just [ "/dog" ] }
  runTest "Common tryExtractCommand /dog 123"
    $ assertEqual
        { actual : C.tryExtractCommand { text : notNull "/dog 123" },
          expected : Just [ "/dog", "123" ] }
