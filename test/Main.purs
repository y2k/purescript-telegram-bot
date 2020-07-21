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
import Effect.Now (nowDateTime)
import Main (unsafeParseJson)
import Partial.Unsafe (unsafePartial)

test_user_login env = do
  msg <- unsafeParseJson """{ "message_id": 209149, "from": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "chat": { "id": -1001130908027, "title": "Programming Offtop", "username": "pofftop", "type": "supergroup" }, "date": 1595360387, "new_chat_participant": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_member": { "id": 714583317, "is_bot": false, "username": "no_name", "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_members": [ { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" } ] }"""
  case update env msg of
    [ DownloadJson _ f ] -> do
      let json = unsafeParse "{ \"data\": { \"image_mp4_url\": \"_VIDEO_URL_\" } }"
      case f $ Right { body: json } of
        [ SendVideo _ _ _ text _ f ] -> do
          assertEqual { expected: Just "@no_name, докажите что вы человек.\nНапишите что происходит на картинке. У вас 30 секунд 😸", actual: text }
          case f 0 of
            [ Delay x ] -> do
              assertEqual { expected: Milliseconds 30_000.0, actual: x.duration }
              case x.commands of
                [ DeleteMessage _ ] -> pure unit
                _ -> fail
            _ -> fail
        _ -> fail
    _ -> fail

test_reroll_after_time env = do
  let msg = 
        { message: notNull { chat: { id: 0 }, from: { id: 0 }, message_id: 0 }
        , data: notNull $ packData "reroll" { id: 0 } "cat" (unsafePartial fromJust (adjust (Milliseconds (-60_000.0)) env.now))
        , from: { id: 0 }, chat: null, text: "", id: 0, new_chat_member: null }
  case update env msg of
    [ DownloadJson _ _ ] -> assertTrue true
    _ -> assertTrue false

test_reroll env = do
  let msg = 
        { message: notNull { chat: { id: 0 }, from: { id: 0 }, message_id: 0 }
        , data: notNull $ packData "reroll" { id: 0 } "cat" env.now
        , from: { id: 0 }, chat: null, text: "", id: 0, new_chat_member: null }
  case update env msg of
    [ DownloadJson url f ] -> do
      assertUrl url
      let json = jsonParser "{ \"data\": { \"image_mp4_url\": \"_VIDEO_URL_\" } }" # unsafePartial fromRight
      case f $ Right { body: json } of
        [ EditVideo x ] -> assertEqual { expected: "_VIDEO_URL_", actual: x.url }
        _ -> assertTrue false
    _ -> assertTrue false

test_cat env = do
  let msg = { from: { id: 0 }, chat: notNull { id: 0 }, text: "/cat", id: 0, new_chat_member: null, data: null, message: null }
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

unsafeParse x = jsonParser x # unsafePartial fromRight
fail = assert' "" false

main :: Effect Unit
main = do
  now <- nowDateTime
  test_cat { apiKey: "_KEY_", now: now }
  test_reroll { apiKey: "_KEY_", now: now }
  test_reroll_after_time { apiKey: "_KEY_", now: now }
  test_user_login { apiKey: "_KEY_", now: now }
