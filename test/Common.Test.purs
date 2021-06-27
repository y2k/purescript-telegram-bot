module Test.Common (main) where

import Prelude

import Common as C
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull)
import Effect (Effect)
import Test.Assert (assertEqual)
import TestUtils (runTest)

main :: Effect Unit
main = do
  runTest "Common tryExtractCommand /dog"
    $ assertEqual
        { actual : C.tryExtractCommand { text : notNull "/dog" },
          expected : Just [ "dog" ] }
  runTest "Common tryExtractCommand /dog@mybot"
    $ assertEqual
        { actual : C.tryExtractCommand { text : notNull "/dog@mybot" },
          expected : Just [ "dog" ] }
  runTest "Common tryExtractCommand /dog 123"
    $ assertEqual
        { actual : C.tryExtractCommand { text : notNull "/dog 123" },
          expected : Just [ "dog", "123" ] }
