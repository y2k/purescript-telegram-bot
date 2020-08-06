module Test.Main (main) where

import Domain
import Prelude
import Test.Assert

import Common (packData)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime (adjust)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable (notNull, null)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import Main (unsafeParseJson)
import Partial.Unsafe (unsafePartial)
import Test.Domain2 as TD2
import Test.PeriodicPostsImages as T3

test_user_login env = do
  msg <- unsafeParseJson """{ "message_id": 209149, "from": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "chat": { "id": -1001130908027, "title": "Programming Offtop", "username": "pofftop", "type": "supergroup" }, "date": 1595360387, "new_chat_participant": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_member": { "id": 714583317, "is_bot": false, "username": "no_name", "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_members": [ { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" } ] }"""
  x <- case update env msg of
        [ DownloadJson x ] -> pure x
        _ -> throw ""
  let json = unsafeParse "{ \"data\": { \"image_mp4_url\": \"_VIDEO_URL_\" } }"
  x <- case x.f $ Right { body: json } of
        [ SendVideo x ] -> pure x
        _ -> throw ""
  assertEqual { expected: Just "@no_name, докажите что вы человек.\nНапишите что происходит на картинке. У вас 30 секунд 😸", actual: x.caption }
  x <- case x.f 0 of
        [ Delay x ] -> pure x
        _ -> throw ""
  assertEqual { expected: Milliseconds 30_000.0, actual: x.duration }
  case x.commands of
    [ DeleteMessage _ ] -> pure unit
    _ -> throw ""

test_reroll_after_time env = do
  let msg = 
        { message: notNull { chat: { id: 0 }, from: { id: 0 }, message_id: 0 }
        , data: notNull $ packData "reroll" { id: 0 } "cat" (unsafePartial fromJust (adjust (Milliseconds (-60_000.0)) env.now))
        , from: { id: 0 }, chat: null, text: notNull "", message_id: notNull 0, new_chat_member: null }
  case update env msg of
    [ DownloadJson _ ] -> assertTrue true
    _ -> assertTrue false

test_reroll env = do
  let msg = 
        { message: notNull { chat: { id: 0 }, from: { id: 0 }, message_id: 0 }
        , data: notNull $ packData "reroll" { id: 0 } "cat" env.now
        , from: { id: 0 }, chat: null, text: notNull "", message_id: notNull 0, new_chat_member: null }
  case update env msg of
    [ DownloadJson x ] -> do
      assertUrl x.url
      let json = jsonParser "{ \"data\": { \"image_mp4_url\": \"_VIDEO_URL_\" } }" # unsafePartial fromRight
      case x.f $ Right { body: json } of
        [ EditVideo x ] -> assertEqual { expected: "_VIDEO_URL_", actual: x.url }
        _ -> assertTrue false
    _ -> assertTrue false

test_cat env = do
  let msg = { from: { id: 0 }, chat: notNull { id: 0 }, text: notNull "/cat", message_id: notNull 0, new_chat_member: null, data: null, message: null }
  case update env msg of
    [ DownloadJson x ] -> do
      assertUrl x.url
      let json = jsonParser "{ \"data\": { \"image_mp4_url\": \"_VIDEO_URL_\" } }" # unsafePartial fromRight
      case x.f $ Right { body: json } of
        [ SendVideo x ] -> assertEqual { expected: "_VIDEO_URL_", actual: x.url }
        _ -> assertTrue false
    _ -> assertTrue false

assertUrl url =
  assertEqual 
    { expected: "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=_KEY_&tag=cat"
    , actual: url }

unsafeParse x = jsonParser x # unsafePartial fromRight

main :: Effect Unit
main = do
  now <- nowDateTime
  test_cat { apiKey: "_KEY_", now: now }
  test_reroll { apiKey: "_KEY_", now: now }
  test_reroll_after_time { apiKey: "_KEY_", now: now }
  test_user_login { apiKey: "_KEY_", now: now }
  TD2.main
  T3.test
