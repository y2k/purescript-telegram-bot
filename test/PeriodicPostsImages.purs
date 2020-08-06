module Test.PeriodicPostsImages where

import Prelude

import Data.Argonaut (jsonParser)
import Data.Either (fromRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Main as M
import Partial.Unsafe (unsafePartial)
import PeriodicPostsImages as I
import Queue as Q
import Test.Assert (assertEqual)

test :: Effect Unit
test = do
  log <- Q.newQueue
  let env = 
       { downloadText: (\_ -> liftEffect $ Q.unsafeReadTextFile "test/resources/rss.xml")
       , sendVideo: (\x -> (M.unsafeToJson x >>= (\x -> Q.push (x) log)) # liftEffect) }
  _ <- launchAff_ $ I.start env I.emptyState
  logA <- Q.toArray log
  assertEqual 
    { expected: [ """{ "chatId": "42", "url": "http://TODO/" }""" ]
    , actual: logA }
  pure unit

unsafeParse x = jsonParser x # unsafePartial fromRight
