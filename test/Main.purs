module Test.Main (main) where

import Domain
import Prelude
import Test.Assert

import Common (packData)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime (adjust)
import Data.Either (Either(..), fromRight)
import Data.Maybe (fromJust)
import Data.Nullable (notNull, null)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (nowDateTime)
import Partial.Unsafe (unsafePartial)

test_reroll_after_time env = do
  let msg = 
        { message: notNull { chat: { id: 0 }, from: { id: 0 }, message_id: 0 }
        , data: notNull $ packData "reroll" { id: 0 } "cat" (unsafePartial fromJust (adjust (Milliseconds (-60_000.0)) env.now))
        , from: { id: 0 }, chat: null, text: "", id: 0, regUserName: null }
  case update env msg of
    [ DownloadJson _ _ ] -> assertTrue true
    _ -> assertTrue false

test_reroll env = do
  let msg = 
        { message: notNull { chat: { id: 0 }, from: { id: 0 }, message_id: 0 }
        , data: notNull $ packData "reroll" { id: 0 } "cat" env.now
        , from: { id: 0 }, chat: null, text: "", id: 0, regUserName: null }
  case update env msg of
    [ DownloadJson url f ] -> do
      assertUrl url
      let json = jsonParser "{ \"data\": { \"image_mp4_url\": \"_VIDEO_URL_\" } }" # unsafePartial fromRight
      case f $ Right { body: json } of
        [ EditVideo x ] -> assertEqual { expected: "_VIDEO_URL_", actual: x.url }
        _ -> assertTrue false
    _ -> assertTrue false

test_cat env = do
  let msg = { from: { id: 0 }, chat: notNull { id: 0 }, text: "/cat", id: 0, regUserName: null, data: null, message: null }
  case update env msg of
    [ DownloadJson url f ] -> do
      assertUrl url
      let json = jsonParser "{ \"data\": { \"image_mp4_url\": \"_VIDEO_URL_\" } }" # unsafePartial fromRight
      case f $ Right { body: json } of
        [ SendVideo _ _ url _ _ _ ] -> assertEqual { expected: "_VIDEO_URL_", actual: url }
        _ -> assertTrue false
    _ -> assertTrue false

assertUrl url =
  assertEqual 
    { expected: "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=_KEY_&tag=cat"
    , actual: url }

main :: Effect Unit
main = do
  now <- nowDateTime
  test_cat { apiKey: "_KEY_", now: now }
  test_reroll { apiKey: "_KEY_", now: now }
  test_reroll_after_time { apiKey: "_KEY_", now: now }
