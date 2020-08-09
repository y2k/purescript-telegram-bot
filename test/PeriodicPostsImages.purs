module Test.PeriodicPostsImages where

import Prelude

import Data.Argonaut (jsonParser)
import Data.Either (fromRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as C
import Main as M
import Partial.Unsafe (unsafePartial)
import PeriodicPostsImages as I
import Queue as Q
import Test.Assert (assertEqual)

test :: Effect Unit
test = do
  C.log $ "\n== PeriodicPostsImages.test ==\n"
  log <- Q.newQueue
  let env = 
       { downloadText: (\_ -> liftEffect $ Q.unsafeReadTextFile "test/resources/rss.xml")
       , sendVideo: (\x -> (M.unsafeToJson x >>= (\x -> Q.push (x) log)) # liftEffect) }
  start <- I.mkStart

  Q.reset log
  _ <- launchAff_ $ start env
  logA <- Q.toArray log
  assertEqual { expected: [], actual: logA }

  Q.reset log
  _ <- launchAff_ $ start $ env { downloadText = (\_ -> liftEffect $ Q.unsafeReadTextFile "test/resources/rss2.xml") }
  logA <- Q.toArray log
  assertEqual 
    { expected: [ """{"chat":"-1001130908027","url":"http://img0.joyreactor.cc/pics/post/mp4/-6086130.mp4"}""" ]
    , actual: logA }

  Q.reset log
  _ <- launchAff_ $ start $ env { downloadText = (\_ -> liftEffect $ Q.unsafeReadTextFile "test/resources/rss2.xml") }
  logA <- Q.toArray log
  assertEqual { expected: [], actual: logA }

unsafeParse x = jsonParser x # unsafePartial fromRight
