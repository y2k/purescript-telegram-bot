module Test.PeriodicPostsImages where

import Prelude

import Effect (Effect)
import Effect.Aff as A
import Effect.Class (liftEffect)
import Main as M
import PeriodicPostsImages as I
import Test.Assert (assertEqual)
import TestUtils (runTest)
import TestUtils as T

main :: Effect Unit
main = do
  runTest "PeriodicPostsImages - test errors handle" do
    let env =
          { downloadText: (\x -> A.throwError (A.error "fake error"))
          , sendVideo: (\x -> pure unit) }
    start <- I.mkStart
    A.launchAff_ $ start env

  runTest "PeriodicPostsImages - no update at start" do
    log <- T.newQueue
    let env =
          { downloadText: (\x -> liftEffect $ T.unsafeReadTextFile $ "test/resources/" <> (show $ T.stringHashCode x.url) <> ".1.xml")
          , sendVideo: (\x -> (M.unsafeToJson x >>= (\x -> T.push x log)) # liftEffect) }
    start <- I.mkStart

    _ <- A.launchAff_ $ start env

    T.reset log
    _ <- A.launchAff_ $ start env
    logA <- T.toArray log
    assertEqual { expected: [], actual: logA }

  runTest "PeriodicPostsImages - no updates after updates" do
    log <- T.newQueue
    let env =
          { downloadText: (\x -> liftEffect $ T.unsafeReadTextFile $ "test/resources/" <> (show $ T.stringHashCode x.url) <> ".1.xml")
          , sendVideo: (\x -> (M.unsafeToJson x >>= (\x -> T.push x log)) # liftEffect) }
    start <- I.mkStart

    _ <- A.launchAff_ $ start env
    _ <- A.launchAff_ $ start $ env { downloadText = (\x -> liftEffect $ T.unsafeReadTextFile $ "test/resources/" <> (show $ T.stringHashCode x.url) <> ".2.xml") }

    T.reset log
    _ <- A.launchAff_ $ start $ env { downloadText = (\x -> liftEffect $ T.unsafeReadTextFile $ "test/resources/" <> (show $ T.stringHashCode x.url) <> ".2.xml") }
    logA <- T.toArray log
    assertEqual { expected: [], actual: logA }

  runTest "PeriodicPostsImages - test" do
    log <- T.newQueue
    let env =
          { downloadText: (\x -> liftEffect $ T.unsafeReadTextFile $ "test/resources/" <> (show $ T.stringHashCode x.url) <> ".1.xml")
          , sendVideo: (\x -> (M.unsafeToJson x >>= (\x -> T.push x log)) # liftEffect) }
    start <- I.mkStart

    T.reset log
    _ <- A.launchAff_ $ start env
    logA <- T.toArray log
    assertEqual { expected: [], actual: logA }

    T.reset log
    _ <- A.launchAff_ $ start $ env { downloadText = (\x -> liftEffect $ T.unsafeReadTextFile $ "test/resources/" <> (show $ T.stringHashCode x.url) <> ".2.xml") }
    logA <- T.toArray log
    assertEqual
      { expected: [ """{"chat":"-1001130908027","url":"http://img0.joyreactor.cc/pics/post/mp4/-6086130.mp4","caption":"#котэ вам в ленту"}""" ]
      , actual: logA }
