module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Domain as T1
import Test.PeriodicPostsImages as T2
import Test.Common as C

main :: Effect Unit
main = do
  T1.main
  T2.main
  C.main
